use crate::flipgraph::Flipgraph;
use itertools::Itertools;
use std::io::Write;

#[derive(Copy, Clone)]
pub enum FlipgraphOutputFormat {
    TabbedTable, CSV
}

pub fn write_flipgraph(g: &Flipgraph, w: &mut dyn Write, format: FlipgraphOutputFormat) -> std::io::Result<()> {
    let levels = g.get_levels();

    let prefix = match format {
        FlipgraphOutputFormat::TabbedTable => "",
        FlipgraphOutputFormat::CSV => "# "
    };

    writeln!(w, "{}", prefix)?;
    writeln!(w, "{}Flipgraph Statistics (n = {})", prefix, g.get_n())?;
    writeln!(w, "{}|V| = {}", prefix, g.node_count())?;
    writeln!(w, "{}|E| = {}", prefix, g.edge_count())?;
    writeln!(w, "{}", prefix)?;


    write_stats_header(w, format)?;
    write_stats_line(&g, w, format,"*", (0..g.node_count()).collect_vec(), )?;
    for (level, indices) in levels.into_iter().sorted_by_key(|&(level, _)| -(level as isize)) {
        write_stats_line(&g, w, format,&format!("{}", level), indices)?;
    }

    Ok(())
}


fn write_stats_header(w: &mut dyn Write, format: FlipgraphOutputFormat) -> std::io::Result<()> {
    match format {
        FlipgraphOutputFormat::TabbedTable =>
            writeln!(w, "{:<10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10}", "#Edges", "#Woods", "MinDeg", "AvgDeg", "MaxDeg", "Min↑Deg", "Max↑Deg", "Min↓Deg", "Max↓Deg", "#Minima"),
        FlipgraphOutputFormat::CSV =>
            writeln!(w, "{};{};{};{};{};{};{};{};{};{}", "Edges", "Woods", "MinDeg", "AvgDeg", "MaxDeg", "Min↑Deg", "Max↑Deg", "Min↓Deg", "Max↓Deg", "Minima")
    }
}

fn write_stats_line(g: &Flipgraph, w: &mut dyn Write, format: FlipgraphOutputFormat, name: &str, nodes: Vec<usize>) -> std::io::Result<()> {

    let degrees = nodes.iter().map(|idx| g.get_neighbors(*idx).count()).collect_vec();
    let down_degrees = nodes.iter().map(|idx| g.get_neighbors(*idx).filter(|&&nb| g.get_level(nb) < g.get_level(*idx)).count()).collect_vec();
    let up_degrees = nodes.iter().map(|idx| g.get_neighbors(*idx).filter(|&&nb| g.get_level(nb) > g.get_level(*idx)).count()).collect_vec();

    let min_degree = degrees.iter().min().unwrap();
    let max_degree = degrees.iter().max().unwrap();

    let minimums = nodes.iter().filter(|idx| g.get_neighbors(**idx).filter(|&&nb| g.get_level(nb) < g.get_level(**idx)).count() == 0).collect_vec();

    let min_up_degree = up_degrees.iter().min().unwrap();
    let max_up_degree = up_degrees.iter().max().unwrap();
    let min_down_degree = down_degrees.iter().min().unwrap();
    let max_down_degree = down_degrees.iter().max().unwrap();
    let avg_degree = degrees.iter().sum::<usize>() as f64 / degrees.len() as f64;

    match format {
        FlipgraphOutputFormat::TabbedTable =>
            writeln!(w, "{:<10} {:>10} {:>10} {:>10.2} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10}", name, nodes.len(), min_degree, avg_degree, max_degree, min_up_degree, max_up_degree, min_down_degree, max_down_degree, minimums.len()),
        FlipgraphOutputFormat::CSV =>
            writeln!(w, "{};{};{};{};{};{};{};{};{};{}", name, nodes.len(), min_degree, avg_degree, max_degree, min_up_degree, max_up_degree, min_down_degree, max_down_degree, minimums.len())
    }
}