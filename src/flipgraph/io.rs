use crate::flipgraph::Flipgraph;
use std::io::Write;
use crate::flipgraph::stats::Stats;

#[derive(Copy, Clone)]
pub enum FlipgraphOutputFormat {
    TabbedTable, CSV
}

pub fn write_flipgraph(g: &Flipgraph, w: &mut dyn Write, format: FlipgraphOutputFormat, with_check: bool) -> std::io::Result<()> {

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
    for stats_entry in g.compute_stats(with_check) {
        write_stats_line(w, format, &stats_entry)?;
    }

    Ok(())
}

pub fn write_random_walk(w: &mut dyn Write, format: FlipgraphOutputFormat, stats: &Vec<Stats>, with_check: bool) -> std::io::Result<()> {
    write_stats_header(w, format)?;
    for stats_entry in stats {
        write_stats_line(w, format, &stats_entry)?;
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

fn write_stats_line(w: &mut dyn Write, format: FlipgraphOutputFormat, stats: &Stats) -> std::io::Result<()> {

    let name = match stats.level {
        None => "*".to_string(),
        Some(nr) => nr.to_string()
    };

    match format {
        FlipgraphOutputFormat::TabbedTable =>
            writeln!(w, "{:<10} {:>10} {:>10} {:>10.2} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10}", name, stats.cardinality, stats.min_deg, stats.avg_deg, stats.max_deg, stats.min_up_deg, stats.max_up_deg, stats.min_down_deg, stats.max_down_deg, stats.minima),
        FlipgraphOutputFormat::CSV =>
            writeln!(w, "{};{};{};{};{};{};{};{};{};{}", name, stats.cardinality, stats.min_deg, stats.avg_deg, stats.max_deg, stats.min_up_deg, stats.max_up_deg, stats.min_down_deg, stats.max_down_deg, stats.minima)
    }

}