use crate::flipgraph::Flipgraph;
use crate::schnyder::figures::{
    max_level, min_degree_lb, min_degree_ub, min_downdegree_lb_by_level,
    min_downdegree_ub_by_level, min_level, min_updegree_lb_by_level, min_updegree_ub_by_level,
};
use itertools::Itertools;
use std::collections::HashMap;

pub struct Stats {
    pub n: u8,
    pub total: StatsLine,
    pub levels: HashMap<u16, StatsLine>,
    pub min_level: u16,
    pub max_level: u16,
}

#[derive(Copy, Clone)]
pub struct StatsLine {
    pub cardinality: usize,
    pub min_deg: usize,
    pub avg_deg: f64,
    pub max_deg: usize,
    pub min_up_deg: usize,
    pub max_up_deg: usize,
    pub min_down_deg: usize,
    pub max_down_deg: usize,
    pub minima: usize,
}

macro_rules! check_eq {
    ($name:literal, $e1: expr, $name1:literal, $e2: expr, $name2: literal) => {
        if $e1 != $e2 {
            println!(
                "Check '{}' failed: {}: {} = {} :{}",
                $name, $name1, $e1, $e2, $name2
            );
        }
    };
}

macro_rules! check_geq {
    ($name:literal, $e1: expr, $name1:literal, $e2: expr, $name2: literal) => {
        if $e1 < $e2 {
            println!(
                "Check '{}' failed: {}: {} >= {} :{}",
                $name, $name1, $e1, $e2, $name2
            );
        }
    };
}

macro_rules! check_leq {
    ($name:literal, $e1: expr, $name1:literal, $e2: expr, $name2: literal) => {
        if $e1 > $e2 {
            println!(
                "Check '{}' failed: {}: {} <= {} :{}",
                $name, $name1, $e1, $e2, $name2
            );
        }
    };
}

impl Stats {
    pub fn check(&self, enveloppe_only: bool) {
        self.enveloppe_check();
        if !enveloppe_only {
            self.inner_check()
        }
    }

    fn enveloppe_check(&self) {
        let n = self.n as usize;
        check_geq!(
            "MINDEG_LB",
            self.total.min_deg as isize,
            "minimum degree",
            min_degree_lb(n),
            "lower bound on minimum degree"
        );
        check_geq!(
            "MINLEVEL_ENV",
            self.min_level as usize,
            "actual minimum level",
            min_level(n),
            "calculated minimum level"
        );
        check_leq!(
            "MAXLEVEL_ENV",
            self.max_level as usize,
            "actual maximum level",
            max_level(n),
            "calculated maximum level"
        );
    }

    fn inner_check(&self) {
        let n = self.n as usize;
        check_eq!(
            "MINLEVEL",
            self.min_level as usize,
            "actual minimum level",
            min_level(n),
            "calculated minimum level"
        );
        check_eq!(
            "MAXLEVEL",
            self.max_level as usize,
            "actual maximum level",
            max_level(n),
            "calculated maximum level"
        );
        check_leq!(
            "MINDEG_UB",
            self.total.min_deg,
            "minimum degree",
            min_degree_ub(n),
            "upper bound on minimum degree"
        );
    }
}

impl StatsLine {
    pub fn check(&self, n: u8, level: u8, enveloppe_only: bool) {
        self.enveloppe_check(n, level);
        if !enveloppe_only {
            self.inner_check(n, level)
        }
    }

    fn enveloppe_check(&self, n: u8, level: u8) {
        let n = n as usize;
        let l = level as usize;

        check_geq!(
            "MINUPDEG_LB",
            self.min_up_deg,
            "minimal up-degree",
            min_updegree_lb_by_level(n, l),
            "lower bound on minimal up-degree"
        );
        check_geq!(
            "MINDOWNDEG_LB",
            self.min_down_deg,
            "minimal down-degree",
            min_downdegree_lb_by_level(n, l),
            "lower bound on minimal down-degree"
        );
    }

    fn inner_check(&self, n: u8, level: u8) {
        let n = n as usize;
        let l = level as usize;

        check_leq!(
            "MINUPDEG_UB",
            self.min_up_deg,
            "minimal up-degree",
            min_updegree_ub_by_level(n, l),
            "upper bound on minimal up-degree"
        );
        check_leq!(
            "MINDOENDEG_UB",
            self.min_down_deg,
            "minimal down-degree",
            min_downdegree_ub_by_level(n, l),
            "upper bound on minimal down-degree"
        );
    }
}

impl Flipgraph {
    pub fn compute_stats(&self) -> Stats {
        let total_stats = self.compute_stats_line(&(0..self.node_count()).collect_vec());
        let level_stats: HashMap<_, _> = self
            .get_levels()
            .iter()
            .map(|(level, vertices)| (*level as u16, self.compute_stats_line(vertices)))
            .collect();
        let min_level = *level_stats.keys().min().unwrap();
        let max_level = *level_stats.keys().max().unwrap();

        Stats {
            n: self.get_n(),
            levels: level_stats,
            total: total_stats,
            min_level,
            max_level,
        }
    }

    fn compute_stats_line(&self, subset: &Vec<usize>) -> StatsLine {
        let degrees = subset
            .iter()
            .map(|idx| self.get_neighbors(*idx).count())
            .collect_vec();
        let down_degrees = subset
            .iter()
            .map(|idx| {
                self.get_neighbors(*idx)
                    .filter(|&&nb| self.get_level(nb) < self.get_level(*idx))
                    .count()
            })
            .collect_vec();
        let up_degrees = subset
            .iter()
            .map(|idx| {
                self.get_neighbors(*idx)
                    .filter(|&&nb| self.get_level(nb) > self.get_level(*idx))
                    .count()
            })
            .collect_vec();

        let min_deg = *degrees.iter().min().unwrap();
        let max_deg = *degrees.iter().max().unwrap();

        let minima = subset
            .iter()
            .filter(|idx| {
                self.get_neighbors(**idx)
                    .filter(|&&nb| self.get_level(nb) < self.get_level(**idx))
                    .count()
                    == 0
            })
            .collect_vec();

        let min_up_deg = *up_degrees.iter().min().unwrap();
        let max_up_deg = *up_degrees.iter().max().unwrap();
        let min_down_deg = *down_degrees.iter().min().unwrap();
        let max_down_deg = *down_degrees.iter().max().unwrap();
        let avg_deg = degrees.iter().sum::<usize>() as f64 / degrees.len() as f64;

        let cardinality = subset.len();

        StatsLine {
            cardinality,
            min_deg,
            avg_deg,
            max_deg,
            min_up_deg,
            max_up_deg,
            min_down_deg,
            max_down_deg,
            minima: minima.len(),
        }
    }
}
