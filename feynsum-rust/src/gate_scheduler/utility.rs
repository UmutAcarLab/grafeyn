use std::collections::HashSet;

use log::debug;

use crate::types::{GateIndex, QubitIndex};

pub fn okay_to_visit<'a>(
    num_gates: usize,
    gate_touches: &[&'a HashSet<QubitIndex>],
    frontier: &[GateIndex],
    gi: GateIndex,
) -> bool {
    gi < num_gates && gate_touches[gi].iter().all(|qi| frontier[*qi] == gi)
}

pub fn mark_as_visit<'a>(
    num_gates: usize,
    gate_touches: &[&'a HashSet<QubitIndex>],
    frontier: &mut Vec<GateIndex>,
    gi: GateIndex,
) {
    debug!("visiting gate: {}", gi);
    assert!(okay_to_visit(num_gates, gate_touches, frontier, gi));
    for qi in gate_touches[gi] {
        let next = next_touch(num_gates, gate_touches, *qi, gi + 1);

        frontier[*qi] = next;
        debug!("updated frontier[{}] to {}", qi, frontier[*qi]);
    }
}

pub fn next_touch(
    num_gates: usize,
    gate_touches: &[&HashSet<QubitIndex>],
    qi: QubitIndex,
    gi: GateIndex,
) -> GateIndex {
    if gi >= num_gates {
        num_gates
    } else if gate_touches[gi].contains(&qi) {
        gi
    } else {
        next_touch(num_gates, gate_touches, qi, gi + 1)
    }
}
