use std::time::{Duration, Instant};

use crate::bytecode::Op;

pub struct Record {
    pub op: Op,
    pub call_count: usize,
}

pub struct Report {
    pub records: Vec<Record>,
    pub exec_time: Duration,
}

pub struct VmProfiler {
    start_time: Instant,
    counters: [usize; 50],
}

impl Default for VmProfiler {
    fn default() -> Self {
        Self {
            counters: [0; 50],
            start_time: Instant::now(),
        }
    }
}

impl VmProfiler {
    pub fn enter(&mut self) {
        self.start_time = Instant::now();
    }

    pub fn record_instruction(&mut self, op: Op) {
        self.counters[(op as u64) as usize] += 1;
    }

    pub fn report(self) -> Report {
        let mut records = vec![];
        let exec_time = Instant::now().duration_since(self.start_time);

        for i in 0..45 {
            if self.counters[i] == 0 {
                continue;
            }

            records.push(Record {
                op: (i as u8).into(),
                call_count: self.counters[i],
            });
        }

        records.sort_by(|a, b| b.call_count.cmp(&a.call_count));

        Report { records, exec_time }
    }
}
