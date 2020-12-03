use std::cmp::max;

pub fn min_level(n: usize) -> usize {
    check_n(n);
    (((3*n - 3) as f64) / 2.0).ceil() as usize
}

pub fn max_level(n: usize) -> usize {
    check_n(n);
    3*n - 6
}

pub fn min_degree_ub(n: usize) -> usize {
    check_n(n);
    2*tnpoo3f(n) - 1
}

pub fn min_degree_lb(n: usize) -> isize {
    check_n(n);
    2*(n as isize) - (tnpoo3f(n) as isize) - 6
}

pub fn min_updegree_lb_by_level(n: usize, level: usize) -> usize {
    check_level(n, level);
    let d = depth(n, level);
    if d <= 3 {
        if n == 5 && d == 3 {
            d + 1
        } else {
            d
        }
    } else {
        2*d - 3
    }
}

pub fn min_updegree_ub_by_level(n: usize, level: usize) -> usize {
    check_level(n, level);
    return min_updegree_lb_by_level(n, level);
}

pub fn min_downdegree_ub_by_level(n: usize, level: usize) -> usize {
    check_level(n, level);
    let d = depth(n, level) as isize;
    let d_c = critical_depth(n) as isize;
    let n = n as isize;
    let value_c = 2*n - 5 - 3*d_c;

    if d <= d_c {
        (2*n - 5 - 3*d) as usize
    } else {
        max(0, value_c - (d - d_c)) as usize
    }
}

pub fn min_downdegree_lb_by_level(n: usize, level: usize) -> usize {
    check_level(n, level);
    let d = depth(n, level) as isize;
    let n = n as isize;
    max(0, 2*n - 5 - 3*d) as usize
}

//
// Private Stuff
//

fn check_n(n: usize) {
    if n < 3 {
        panic!("n must always be greater or equal to 3.")
    }
}

fn check_level(n: usize, level: usize) {
    check_n(n);
    if level < min_level(n) {
        panic!("level too low.");
    }
    if level > max_level(n) {
        panic!("level too high.");
    }
}

fn tnpoo3f(n: usize) -> usize {
    (((2*n + 1)as f64)/3.0).floor() as usize
}

fn depth(n: usize, level: usize) -> usize {
    (3*n - 6) - level
}

fn critical_depth(n: usize) -> usize {
    tnpoo3f(n) - 3
}