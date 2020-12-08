use crate::flipgraph::Flipgraph;
use itertools::Itertools;
use crate::schnyder::figures::{min_level, max_level, min_degree_lb, min_degree_ub, min_updegree_lb_by_level, min_updegree_ub_by_level, min_downdegree_lb_by_level, min_downdegree_ub_by_level};

pub struct Stats {
    pub level: Option<u8>,
    pub cardinality: usize,
    pub min_deg: usize,
    pub avg_deg: f64,
    pub max_deg: usize,
    pub min_up_deg: usize,
    pub max_up_deg: usize,
    pub min_down_deg: usize,
    pub max_down_deg: usize,
    pub minima: usize
}

impl Flipgraph {

    pub fn compute_stats(&self, with_check: bool) -> Vec<Stats> {
        let levels = self.get_levels();

        let mut result = Vec::with_capacity(levels.len() + 1);
        result.push(self.compute_stats_for_subset(&(0..self.node_count()).collect_vec(), None, with_check));

        for level in levels.keys().sorted_by_key(|l| -(**l as i16)) {
            result.push(self.compute_stats_for_subset(&levels.get(level).unwrap(), Some(*level), with_check));
        }

        if with_check {
            let n = self.get_n();
            assert_eq!(*levels.keys().min().unwrap(), min_level(n) as u8);
            assert_eq!(*levels.keys().max().unwrap(), max_level(n) as u8);
            assert!(result[0].min_deg as isize >= min_degree_lb(n));
            assert!(result[0].min_deg <= min_degree_ub(n));
        }

        result
    }

    fn compute_stats_for_subset(&self, subset: &Vec<usize>, level: Option<u8>, with_check: bool) -> Stats {

        let degrees = subset.iter().map(|idx| self.get_neighbors(*idx).count()).collect_vec();
        let down_degrees = subset.iter().map(|idx| self.get_neighbors(*idx).filter(|&&nb| self.get_level(nb) < self.get_level(*idx)).count()).collect_vec();
        let up_degrees = subset.iter().map(|idx| self.get_neighbors(*idx).filter(|&&nb| self.get_level(nb) > self.get_level(*idx)).count()).collect_vec();

        let min_deg = *degrees.iter().min().unwrap();
        let max_deg = *degrees.iter().max().unwrap();

        let minima = subset.iter().filter(|idx| self.get_neighbors(**idx).filter(|&&nb| self.get_level(nb) < self.get_level(**idx)).count() == 0).collect_vec();

        let min_up_deg = *up_degrees.iter().min().unwrap();
        let max_up_deg = *up_degrees.iter().max().unwrap();
        let min_down_deg = *down_degrees.iter().min().unwrap();
        let max_down_deg = *down_degrees.iter().max().unwrap();
        let avg_deg = degrees.iter().sum::<usize>() as f64 / degrees.len() as f64;

        let cardinality = subset.len();

        if with_check {
            if let Some(l) = level {
                let n = self.get_n();
                let l = l as usize;

                assert!(min_up_deg >= min_updegree_lb_by_level(n, l));
                assert!(min_up_deg <= min_updegree_ub_by_level(n, l));
                assert!(min_down_deg >= min_downdegree_lb_by_level(n, l));
                assert!(min_down_deg <= min_downdegree_ub_by_level(n, l));
            }
        }

        Stats {
            level,
            cardinality, min_deg, avg_deg, max_deg,
            min_up_deg, max_up_deg,
            min_down_deg, max_down_deg,
            minima: minima.len()
        }
    }

}