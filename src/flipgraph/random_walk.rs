use itertools::Itertools;
use rand::{thread_rng, Rng};
use std::cmp::{max, min};
use std::thread;
use std::time::{Duration, Instant};

use super::stats::StatsLine;
use crate::flipgraph::stats::Stats;
use crate::schnyder::algorithm::Operation;
use crate::schnyder::enums::SchnyderColor;
use crate::schnyder::SchnyderMap;

impl StatsLine {
    pub fn neutral() -> StatsLine {
        StatsLine {
            cardinality: 0,
            min_deg: usize::max_value(),
            avg_deg: 0f64,
            max_deg: usize::min_value(),
            min_up_deg: usize::max_value(),
            max_up_deg: usize::min_value(),
            min_down_deg: usize::max_value(),
            max_down_deg: usize::min_value(),
            minima: 0,
        }
    }

    pub fn from_admissible_ops(admissible_ops: &Vec<Operation>) -> StatsLine {
        let up = admissible_ops.iter().filter(|op| op.is_upwards()).count();
        let down = admissible_ops.iter().filter(|op| op.is_downwards()).count();

        StatsLine {
            cardinality: 1,
            min_deg: admissible_ops.len(),
            avg_deg: admissible_ops.len() as f64,
            max_deg: admissible_ops.len(),
            min_up_deg: up,
            max_up_deg: up,
            min_down_deg: down,
            max_down_deg: down,
            minima: if down == 0 { 1 } else { 0 },
        }
    }

    pub fn reduce(&mut self, other: &StatsLine) {
        let n1 = self.cardinality as f64;
        let n2 = other.cardinality as f64;

        self.cardinality += other.cardinality;
        self.min_deg = min(self.min_deg, other.min_deg);

        self.avg_deg = (self.avg_deg * n1 + other.avg_deg * n2) / (n1 + n2);

        self.max_deg = max(self.max_deg, other.max_deg);
        self.min_up_deg = min(self.min_up_deg, other.min_up_deg);
        self.max_up_deg = max(self.max_up_deg, other.max_up_deg);
        self.min_down_deg = min(self.min_down_deg, other.min_down_deg);
        self.max_down_deg = max(self.max_down_deg, other.max_down_deg);
        self.minima += other.minima;
    }
}

pub fn random_walk(
    n: usize,
    thread_count: usize,
    time_limit: Option<Duration>,
    sample_limit: Option<usize>,
) -> Stats {
    if n < 3 {
        panic!("n must be at least 3.");
    }

    if time_limit.is_none() && sample_limit.is_none() {
        panic!("Neither time nor sample limit provided.");
    }

    let mut handles = Vec::new();
    let min_level = crate::schnyder::figures::min_level(n);
    let max_level = crate::schnyder::figures::max_level(n);
    let number_of_levels = max_level - min_level + 1;

    for _ in 0..thread_count {
        let walk_start_instant = Instant::now();
        let mut sample_count = 0;
        //
        let mut last_log = Instant::now();
        let mut samples_since_last_log = 0;
        //
        let mut sample_bins = (0..number_of_levels)
            .map(|_| StatsLine::neutral())
            .collect_vec();
        let mut overall_sample_bin = StatsLine::neutral();

        let handle = thread::spawn(move || {
            let mut rand = thread_rng();
            let mut current = vec![
                SchnyderMap::build_simple_stack(n, SchnyderColor::Red).expect("TODO"),
                SchnyderMap::build_simple_stack(n, SchnyderColor::Green).expect("TODO"),
                SchnyderMap::build_simple_stack(n, SchnyderColor::Blue).expect("TODO"),
                SchnyderMap::build_min_edge_wood(n, SchnyderColor::Red).expect("TODO"),
                SchnyderMap::build_min_edge_wood(n, SchnyderColor::Green).expect("TODO"),
                SchnyderMap::build_min_edge_wood(n, SchnyderColor::Blue).expect("TODO"),
            ];

            loop {
                for cur in current.iter_mut() {
                    let admissible_ops = cur.get_admissible_ops().expect("TODO");

                    let sample = StatsLine::from_admissible_ops(&admissible_ops);
                    overall_sample_bin.reduce(&sample);
                    sample_bins[cur.map.edge_count() - min_level].reduce(&sample);

                    let pick = rand.gen_range(0, admissible_ops.len());
                    cur.exec_op(&admissible_ops[pick]).expect("TODO");
                    sample_count += 1;
                    samples_since_last_log += 1;
                }

                if last_log.elapsed().as_millis() >= 5000 {
                    println!("Samples per sec = {}", samples_since_last_log as f64 / 5.0);
                    last_log = Instant::now();
                    samples_since_last_log = 0;
                }

                if let Some(time_limit) = time_limit {
                    if walk_start_instant.elapsed() > time_limit {
                        break;
                    }
                }

                if let Some(sample_limit) = sample_limit {
                    if sample_count > sample_limit {
                        break;
                    }
                }
            }

            sample_bins.push(overall_sample_bin);
            //sample_bins.reverse();

            sample_bins
        });

        handles.push(handle);
    }

    let mut sample_bins = (0..=number_of_levels)
        .map(|_| StatsLine::neutral())
        .collect_vec();

    for handle in handles {
        let bins = handle.join().unwrap();
        for i in 0..sample_bins.len() {
            sample_bins[i].reduce(&bins[i]);
        }
    }

    let stats = Stats {
        n: n as u8,
        min_level: min_level as u16,
        max_level: max_level as u16,
        levels: (0..number_of_levels)
            .map(|l| ((l + min_level) as u16, sample_bins[l]))
            .collect(),
        total: sample_bins[number_of_levels],
    };

    return stats;
}
