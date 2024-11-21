from numpy.typing import NDArray
import qiskit as Q
from qiskit.transpiler.passes import RemoveBarriers
import rustworkx as rx
import numpy as np
import sys
import json
from typing import Any, Iterator, Optional
import time

#import scipy
#def cluster(dag):
#    adjmtx = rx.adjacency_matrix(dag)
#    adjid = np.identity(adjmtx.shape[0])
#    whitened = scipy.cluster.vq.whiten(adjmtx + adjid)
#    return scipy.cluster.vq.kmeans2(whitened, adjmtx.shape[0]//2, minit='points')[1]

EPS = 1e-10

def circuit_to_dag(circ: Q.circuit.QuantumCircuit):
    return Q.converters.circuit_to_dag(circ)

def circuit_to_dep_graph(circ: Q.circuit.QuantumCircuit) -> Q.dagcircuit.DAGCircuit:
    dag = circuit_to_dag(circ)
    dag._multi_graph = dag_to_dep_graph(dag._multi_graph, circuit_find_qubit_dict(circ))
    return dag

def circuit_find_qubit_dict(circ: Q.circuit.QuantumCircuit) -> dict[Q.circuit.Bit, int]:
    return {x: circ.find_bit(x)[0] for x in circ.qubits}

def indirect_reachability(dag):
    "Returns a mapping from nodes to sets of nodes reachable from them, via 2 or more edges"
    reaches = dict()
    def visit(node):
        nodes = {node}
        reaches[node] = nodes
        outs = dag.out_edges(node)
        for _, to, _ in outs:
            if to not in reaches:
                visit(to)
            nodes |= reaches[to] | ({i for _, i, _ in dag.out_edges(to)} - set(outs))
    for node in dag.node_indices():
        visit(node)
    return reaches

def trim_edges(dag):
    todo = set(dag.edge_list())
    indirect_reaches = indirect_reachability(dag)

    def get_id(node):
        if '_node_id' in dir(node):
            return node._node_id
        elif 'node_id' in dir(node):
            return node.node_id
    
    while todo:
        edge = todo.pop()
        gai, gbi = edge
        if any(gbi in indirect_reaches[get_id(gci)] and gbi != get_id(gci) for gci in dag.successors(gai)):
            dag.remove_edge(gai, gbi)

def is_op_node(node: Q.dagcircuit.DAGNode) -> bool:
    return isinstance(node, Q.dagcircuit.DAGOpNode)

def dag_to_dep_graph(dag: Q.dagcircuit.DAGCircuit,
                     find_qubit: dict[Q.circuit.Bit, int],
                     trim=False) -> rx.PyDAG:
    assert isinstance(dag, rx.PyDAG), \
        f"dag_to_dep_graph expected a DAG of type rustworkx.PyDAG, but got {type(dag)} instead"
    graph = rx.PyDAG(multigraph=False)
    graph.add_nodes_from(dag.nodes())
    reaches = {i: {i} for i in dag.node_indices()}
    todo = set()
    for gai, gbi, qi in dag.weighted_edge_list():
        graph.add_edge(gai, gbi, None)
        if is_op_node(graph.get_node_data(gai)) and is_op_node(graph.get_node_data(gbi)):
            todo.add((gai, gbi))
    visited = set()

    def maybe_push_edge(ifm, ito, gfm, gto):
        """
        Pushes an edge to `todo` if we need to visit it
        Args:
          ifm = index of source gate
          ito = index of target gate
          gfm = source gate
          gto = target gate
        """

        opfm = is_op_node(gfm)
        opto = is_op_node(gto)
        
        if opfm and opto:
            shared = any(find_qubit[q1] == find_qubit[q2]
                         for q1 in gfm.qargs for q2 in gto.qargs)
            if shared and (ifm, ito) not in todo \
               and (ifm, ito) not in visited \
               and (not trim or ito not in reaches[ifm]):
                graph.add_edge(ifm, ito, None)
                todo.add((ifm, ito))
        elif (opfm and any(find_qubit[q] == find_qubit[gto.wire] for q in gfm.qargs)) or \
             (opto and any(find_qubit[q] == find_qubit[gfm.wire] for q in gto.qargs)) or \
             (not opfm and not opto and gfm.wire == gto.wire):
            graph.add_edge(ifm, ito, None)

    while todo:
        gai, gbi = todo.pop()
        visited.add((gai, gbi))
        ga = graph.get_node_data(gai)
        gb = graph.get_node_data(gbi)
        if commutes(ga, gb, find_qubit):
            graph.remove_edge(gai, gbi)
            # Propagate in-edges
            for pidx, _, _ in graph.in_edges(gai):
                maybe_push_edge(pidx, gbi, graph.get_node_data(pidx), gb)
            # Propagate out-edges
            for _, cidx, _ in graph.out_edges(gbi):
                maybe_push_edge(gai, cidx, ga, graph.get_node_data(cidx))
        elif trim:
            reaches[gai] |= reaches[gbi]
    return graph

def as_bits(n: int, nbits: Optional[int] = None) -> list[int]:
    "Converts an integer into its bit representation"
    return [int(bool(n & (1 << (i - 1)))) for i in range(nbits or n.bit_length(), 0, -1)]

def rearrange_gate(mat: NDArray, old: list[int], new: list[int]) -> NDArray:
    """
    Rearranges a gate's unitary matrix for application to a new set of qubits.
    Assumes set(old) = set(new).
    """
    old = list(reversed(old))
    new = list(reversed(new))
    qubits = len(new)
    old_idx = {o:i for i, o in enumerate(old)}
    new_idx = {n:i for i, n in enumerate(new)}
    old2new_idx = {o:new_idx[o] for o in old}
    new2old_idx = {n:old_idx[n] for n in new}

    size = 1 << qubits
    I = np.identity(size, dtype=np.dtype('int64'))
    
    bit_map = np.array([I[new2old_idx[n]] for n in new])
    bit_mat = np.array([as_bits(i, qubits) for i in range(size)])
    mapped = bit_mat @ bit_map
    for i in range(qubits - 1):
        mapped[:, i] <<= qubits - i - 1
    reordered = mapped.sum(axis=1)

    mat1 = np.ndarray(mat.shape, dtype=mat.dtype)
    mat2 = np.ndarray(mat.shape, dtype=mat.dtype)
    
    for i in range(size):
        mat1[i, :] = mat[reordered[i], :]
    for i in range(size):
        mat2[:, i] = mat1[:, reordered[i]]
    return mat2

def align_gates(ga, gb, find_qubit: dict[Q.circuit.Bit, int]):
    """
    Aligns gates along the same qubits, returning a tuple of their new unitaries
    """
    ma = ga.op.to_matrix()
    mb = gb.op.to_matrix()
    qas = [find_qubit[qi] for qi in ga.qargs]
    qbs = [find_qubit[qi] for qi in gb.qargs]
    qas_ins = list(set(qbs) - set(qas))
    qbs_ins = list(set(qas) - set(qbs))
    qas2 = qas + qas_ins
    qbs2 = qbs + qbs_ins
    # Add additional qubits from gb
    ma2 = np.kron(np.identity(1 << len(qas_ins), dtype=ma.dtype), ma)
    # Add additional qubits from ga
    mb2 = np.kron(np.identity(1 << len(qbs_ins), dtype=mb.dtype), mb)
    # Rearrange mb2 to match ma2's qubit order
    mb3 = rearrange_gate(mb2, qbs2, qas2)
    return (ma2, mb3)

def hardcoded_commutes(ga: Q.circuit.Gate, gb: Q.circuit.Gate, find_qubit: dict[Q.circuit.Bit, int]) -> int:
    "Returns 0 if N/A, 1 if commute, 2 if dependent"
    NA, COMMUTE, DEPENDENT = 0, 1, 2
    gan, gbn = ga.op.name, gb.op.name
    qas, qbs = [find_qubit[q] for q in ga.qargs], [find_qubit[q] for q in gb.qargs]

    def comm(gan, gbn, qas, qbs):
        if gan == 'cx' and gbn in ['x', 'sx', 'rx']:
            return qas[1] == qbs[0]
        elif gan == 'cx' and gbn in ['z', 'rz']:
            return qas[0] == qbs[0]
        elif gan == 'cx' and gbn == 'cx':
            return qas[0] != qbs[1] and qas[1] != qbs[0]
        elif gan == 'ccx' and gbn == 'cx':
            return qas[2] != qbs[0] and qbs[1] not in qas[:2]
        elif gan == 'ccx' and gbn == 'ccx':
            return qas[2] not in qbs[:2] and qbs[2] not in qas[:2]
        elif gan == 'ccx' and gbn in ['x', 'sx', 'rx']:
            return qbs[0] not in qas[:2]
        elif gan == 'ccx' and gbn in ['z', 'rz']:
            return qbs[0] in qas[:2]

    if gan == gbn and qas == qbs:
        return COMMUTE
    else:
        c = comm(gan, gbn, qas, qbs)
        return NA if c is None else 2 - c

def commutes(ga: Q.circuit.Gate, gb: Q.circuit.Gate,
             find_qubit: dict[Q.circuit.Bit, int]) -> bool:
    hc = hardcoded_commutes(ga, gb, find_qubit) or hardcoded_commutes(gb, ga, find_qubit)
    if hc == 0: # we don't have this case in the hardcoded rules
        ma, mb = align_gates(ga, gb, find_qubit)
        return (np.abs((ma @ mb) - (mb @ ma)) < EPS).all()
    return bool(hc % 2) # we've have this case in the hardcoded rules

def read_qasm(fh) -> Q.circuit.QuantumCircuit:
    #return Q.QuantumCircuit.from_qasm_file(fh)
    acc = []
    for line in fh:
        if not line.startswith('//'):
            acc.append(line)
    return Q.QuantumCircuit.from_qasm_str(''.join(acc))

def write_dep_graph(num_qubits: int, graph, find_qubit: dict[Q.circuit.Bit, int], fh):
    nodemap = dict()
    numnodes = 0
    nodes = []
    edges = []
    for node in graph.node_indices():
        data = graph.get_node_data(node)
        if is_op_node(data):
            nodemap[node] = numnodes
            numnodes += 1
            nodes.append(data)
    for fm, to in graph.edge_list():
        if is_op_node(graph.get_node_data(fm)) and is_op_node(graph.get_node_data(to)):
            edges.append((nodemap[fm], nodemap[to]))
    
    # TODO: handle cargs
    node_data = []
    for node in nodes:
        if node.op.params:
            node_data.append({
                'name': node.op.name,
                'params': node.op.params,
                'qargs': [find_qubit[qarg] for qarg in node.qargs],
                #'cargs': [f'{}' for carg in node.cargs]
            })
        else:
            node_data.append({
                'name': node.op.name,
                'qargs': [find_qubit[qarg] for qarg in node.qargs],
                #'cargs': [f'{}' for carg in node.cargs]
            })
    data = {'qubits': num_qubits, 'nodes': node_data, 'edges': edges}
    json.dump(data, fh)
    

def usage(argv):
    return f"""
Usage:
    {argv[0]} [--dag] [input.qasm] [output.json]
If either arg is omitted, read from stdin/stdout
If --dag, immediately output unprocessed DAG
"""

def main(argv):
    just_dag = False
    if len(argv) == 2:
        ifh = open(argv[1])
        ofh = sys.stdout
    elif len(argv) == 3:
        ifh = open(argv[1])
        ofh = open(argv[2], 'w')
    elif len(argv) == 4 and argv[1] == '--dag':
        just_dag = True
        ifh = open(argv[2])
        ofh = open(argv[3], 'w')
    else:
        print(usage(argv).strip(), file=sys.stderr)
        return 1
    circuit = read_qasm(ifh)
    circuit.remove_final_measurements(True)
    rb = RemoveBarriers()
    # TODO: prevent commuting across barriers
    circuit = rb(circuit)
    dag = circuit_to_dag(circuit)
    find_qubit = circuit_find_qubit_dict(circuit)
    if just_dag:
        write_dep_graph(circuit.num_qubits, dag._multi_graph, find_qubit, ofh)
    else:
        dg = dag_to_dep_graph(dag._multi_graph, find_qubit, trim=True)
        trim_edges(dg)
        write_dep_graph(circuit.num_qubits, dg, find_qubit, ofh)

# def glen(generator: Iterator[Any]) -> int:
#     return sum(1 for _ in generator)

# def op_edges(g: Q.dagcircuit.DAGCircuit) -> int:
#     return glen(filter(lambda e: isinstance(e[0], Q.dagcircuit.DAGOpNode) and isinstance(e[1], Q.dagcircuit.DAGOpNode), g.edges()))

# def main(argv):
#     if len(argv) == 2:
#         circuit = read_qasm(argv[1])
#         my_dep = circuit_to_dag(circuit)
#         qk_dag = circuit_to_dag(circuit)
#         orig_depth = my_dep.depth()
#         orig_edges = glen(my_dep.edges())
#         print(f"Original DAG: {orig_depth - 1} depth, {orig_edges} edges")

#         TRIM = False
#         my_start = time.time()
#         my_dep._multi_graph = dag_to_dep_graph(my_dep._multi_graph, circuit_find_qubit_dict(circuit), TRIM)
#         my_end = time.time()

#         if TRIM:
#             trim_start = time.time()
#             trim_edges(my_dep._multi_graph)
#             trim_end = time.time()
#             print(f"My dependency graph: {my_dep.depth() - 1} depth, {op_edges(my_dep)} edges, {my_end - my_start:0.4f} sec + trimming for {trim_end - trim_start:0.4f} sec")
#         else:
#             print(f"My dependency graph: {my_dep.depth() - 1} depth, {op_edges(my_dep)} edges, {my_end - my_start:0.4f} sec")
#         qk_start = time.time()
#         qk_dep = Q.converters.dag_to_dagdependency(qk_dag)
#         qk_end = time.time()

#         print(f"Qiskit dependency graph: {qk_dep.depth()} depth, {glen(qk_dep.get_all_edges())} edges, {qk_end - qk_start:0.4f} sec")
#     else:
#         print("Pass a .qasm file as arg", file=sys.stderr)

if __name__ == '__main__':
     exitcode = main(sys.argv)
     exit(exitcode)
