use crate::flipgraph::stats::{Stats, StatsLine};
use crate::flipgraph::Flipgraph;
use std::io::Write;

#[derive(Copy, Clone)]
pub enum FlipgraphOutputFormat {
    TabbedTable,
    CSV,
}

#[derive(Copy, Clone)]
pub enum HeaderType {
    Exact,
    RandomWalk,
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
    writeln!(w, "{}Flip graph Statistics (n = {})", prefix, g.get_n())?;
    writeln!(w, "{}|V| = {}", prefix, g.node_count())?;
    writeln!(w, "{}|E| = {}", prefix, g.edge_count())?;
    writeln!(w, "{}", prefix)?;

    let stats = g.compute_stats();
    write_stats(w, &stats, format, HeaderType::Exact)?;
    if with_check {
        stats.check(false);
    }

    Ok(())
}

pub fn write_random_walk(
    stats: &Stats,
    w: &mut dyn Write,
    format: FlipgraphOutputFormat,
    with_check: bool,
) -> std::io::Result<()> {
    let prefix = match format {
        FlipgraphOutputFormat::TabbedTable => "",
        FlipgraphOutputFormat::CSV => "# ",
    };

    writeln!(w, "{}", prefix)?;
    writeln!(w, "{}Random Walk Statistics (n = {})", prefix, stats.n)?;
    writeln!(w, "{}", prefix)?;

    write_stats(w, &stats, format, HeaderType::RandomWalk)?;
    if with_check {
        stats.check(true);
    }

    Ok(())
}

fn write_stats(
    w: &mut dyn Write,
    stats: &Stats,
    format: FlipgraphOutputFormat,
    header_type: HeaderType,
) -> std::io::Result<()> {
    write_stats_header(w, format, header_type)?;
    write_stats_line(w, &stats.total, format, "*")?;
    for level in (stats.min_level..=stats.max_level).rev() {
        write_stats_line(w, &stats.levels[&level], format, &level.to_string())?;
    }

    Ok(())
}

fn write_stats_header(
    w: &mut dyn Write,
    format: FlipgraphOutputFormat,
    header_type: HeaderType,
) -> std::io::Result<()> {
    let count_str = match header_type {
        HeaderType::Exact => "Woods",
        HeaderType::RandomWalk => "Samples",
    };
    let min_str = match header_type {
        HeaderType::Exact => "Minimus",
        HeaderType::RandomWalk => "Sampled Minimums",
    };

    match format {
        FlipgraphOutputFormat::TabbedTable => writeln!(
            w,
            "{:<10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10}",
            "#Edges",
            &format!("#{}", count_str),
            "MinDeg",
            "AvgDeg",
            "MaxDeg",
            "Min↑Deg",
            "Max↑Deg",
            "Min↓Deg",
            "Max↓Deg",
            &format!("#{}", min_str)
        ),
        FlipgraphOutputFormat::CSV => writeln!(
            w,
            "{};{};{};{};{};{};{};{};{};{}",
            "Edges",
            count_str,
            "MinDeg",
            "AvgDeg",
            "MaxDeg",
            "MinUpDeg",
            "MaxUpDeg",
            "MinDownDeg",
            "MaxDownDeg",
            min_str
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
