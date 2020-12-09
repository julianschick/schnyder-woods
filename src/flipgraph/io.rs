use crate::flipgraph::stats::{Stats, StatsLine};
use crate::flipgraph::Flipgraph;
use std::io::Write;

#[derive(Copy, Clone)]
pub enum FlipgraphOutputFormat {
    TabbedTable,
    CSV,
}

pub fn write_flipgraph(
    g: &Flipgraph,
    w: &mut dyn Write,
    format: FlipgraphOutputFormat,
    with_check: bool,
) -> std::io::Result<()> {
    let prefix = match format {
        FlipgraphOutputFormat::TabbedTable => "",
        FlipgraphOutputFormat::CSV => "# ",
    };

    writeln!(w, "{}", prefix)?;
    writeln!(w, "{}Flipgraph Statistics (n = {})", prefix, g.get_n())?;
    writeln!(w, "{}|V| = {}", prefix, g.node_count())?;
    writeln!(w, "{}|E| = {}", prefix, g.edge_count())?;
    writeln!(w, "{}", prefix)?;

    let stats = g.compute_stats();
    write_stats(w, &stats, format)?;
    if with_check {
        stats.check(false);
    }

    Ok(())
}

/*pub fn write_random_walk(w: &mut dyn Write, format: FlipgraphOutputFormat, stats: &Vec<Stats>, with_check: bool) -> std::io::Result<()> {
    write_stats_header(w, format)?;
    for stats_entry in stats {
        write_stats_line(w, format, &stats_entry)?;
    }

    Ok(())
}*/

fn write_stats(
    w: &mut dyn Write,
    stats: &Stats,
    format: FlipgraphOutputFormat,
) -> std::io::Result<()> {
    write_stats_header(w, format)?;
    write_stats_line(w, &stats.total, format, "*")?;
    for level in stats.min_level..=stats.max_level {
        write_stats_line(w, &stats.levels[&level], format, &level.to_string())?;
    }

    Ok(())
}

fn write_stats_header(w: &mut dyn Write, format: FlipgraphOutputFormat) -> std::io::Result<()> {
    match format {
        FlipgraphOutputFormat::TabbedTable => writeln!(
            w,
            "{:<10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10}",
            "#Edges",
            "#Woods",
            "MinDeg",
            "AvgDeg",
            "MaxDeg",
            "Min↑Deg",
            "Max↑Deg",
            "Min↓Deg",
            "Max↓Deg",
            "#Minimums"
        ),
        FlipgraphOutputFormat::CSV => writeln!(
            w,
            "{};{};{};{};{};{};{};{};{};{}",
            "Edges",
            "Woods",
            "MinDeg",
            "AvgDeg",
            "MaxDeg",
            "MinUpDeg",
            "MaxUpDeg",
            "MinDownDeg",
            "MaxDownDeg",
            "Minimums"
        ),
    }
}

fn write_stats_line(
    w: &mut dyn Write,
    stats: &StatsLine,
    format: FlipgraphOutputFormat,
    name: &str,
) -> std::io::Result<()> {
    match format {
        FlipgraphOutputFormat::TabbedTable => writeln!(
            w,
            "{:<10} {:>10} {:>10} {:>10.2} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10}",
            name,
            stats.cardinality,
            stats.min_deg,
            stats.avg_deg,
            stats.max_deg,
            stats.min_up_deg,
            stats.max_up_deg,
            stats.min_down_deg,
            stats.max_down_deg,
            stats.minima
        ),
        FlipgraphOutputFormat::CSV => writeln!(
            w,
            "{};{};{};{};{};{};{};{};{};{}",
            name,
            stats.cardinality,
            stats.min_deg,
            stats.avg_deg,
            stats.max_deg,
            stats.min_up_deg,
            stats.max_up_deg,
            stats.min_down_deg,
            stats.max_down_deg,
            stats.minima
        ),
    }
}
