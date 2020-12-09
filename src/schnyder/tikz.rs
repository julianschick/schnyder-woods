use crate::graph::enums::Signum;
use crate::graph::indices::VertexI;
use crate::schnyder::SchnyderEdgeDirection::{Bicolored, Unicolored};
use crate::schnyder::{SchnyderColor, SchnyderMap, SchnyderVertexType};
use std::collections::HashMap;
use std::io::Write;

pub struct TikzOptions<'a> {
    pub print_document: bool,
    pub print_environment: bool,
    pub print_styles: bool,
    //
    pub print_face_labels: bool,
    //
    pub anchor: Option<&'a str>,
    pub title: Option<&'a str>,
    //
    pub face_count: Option<&'a HashMap<VertexI, (usize, usize, usize)>>,
    pub drop_coord: SchnyderColor,
    pub slanted: bool,
}

impl<'a> Default for TikzOptions<'a> {
    fn default() -> Self {
        TikzOptions {
            print_document: true,
            print_environment: true,
            print_styles: true,
            print_face_labels: false,
            anchor: None,
            title: None,
            face_count: None,
            drop_coord: SchnyderColor::Blue,
            slanted: false,
        }
    }
}

fn color_to_tikz(color: &SchnyderColor) -> &str {
    match color {
        SchnyderColor::Red => "sws_red",
        SchnyderColor::Green => "sws_green",
        SchnyderColor::Blue => "sws_blue",
    }
}

impl SchnyderMap {
    pub fn write_tikz(&self, writer: &mut dyn Write, options: &TikzOptions) -> std::io::Result<()> {
        let tikz_str = self.generate_tikz(options);
        writer.write_all(tikz_str.as_bytes())
    }

    pub fn generate_tikz(&self, options: &TikzOptions) -> String {
        let local_fc = match options.face_count {
            Some(_) => HashMap::new(),
            None => self.calculate_face_counts(),
        };

        let face_counts = match options.face_count {
            Some(x) => x,
            None => &local_fc,
        };

        let mut mid = String::new();
        let indent_counter = if options.print_document { 1 } else { 0 }
            + if options.print_environment { 1 } else { 0 };
        let ind = "\t".repeat(indent_counter);

        if options.print_styles {
            mid.push_str(&format!("{}\\tikzset {{\n", ind));
            mid.push_str(&format!("{}\tsws_edges_tail/.style = {{}},\n", ind));
            mid.push_str(&format!("{}\tsws_edges_head/.style = {{-latex}},\n", ind));
            mid.push_str(&format!("{}\tsws_bi_edges/.style = {{-latex}},\n", ind));
            mid.push_str(&format!("{}\tsws_red/.style = {{color=red}},\n", ind));
            mid.push_str(&format!(
                "{}\tsws_green/.style = {{color=green!80!black}},\n",
                ind
            ));
            mid.push_str(&format!("{}\tsws_blue/.style = {{color=blue}},\n", ind));
            mid.push_str(&format!(
                "{}\tsws_red_suspension_vertex/.style = {{circle, fill=black, scale=0.2}},\n",
                ind
            ));
            mid.push_str(&format!(
                "{}\tsws_green_suspension_vertex/.style = {{circle, fill=black, scale=0.2}},\n",
                ind
            ));
            mid.push_str(&format!(
                "{}\tsws_blue_suspension_vertex/.style = {{circle, fill=black, scale=0.2}},\n",
                ind
            ));
            mid.push_str(&format!(
                "{}\tsws_normal_vertex/.style = {{circle, fill=black, scale=0.2}},\n",
                ind
            ));
            mid.push_str(&format!("{}}}\n", ind));
        }

        let positions: HashMap<_, _> = face_counts
            .iter()
            .map(|(v, (r, g, b))| {
                (
                    v,
                    match options.drop_coord {
                        SchnyderColor::Red => (b, g),
                        SchnyderColor::Green => (b, r),
                        SchnyderColor::Blue => (g, r),
                    },
                )
            })
            .collect();

        let max_x = self
            .map
            .vertex_indices()
            .map(|v| positions.get(v).unwrap().0)
            .max()
            .unwrap();
        let max_y = self
            .map
            .vertex_indices()
            .map(|v| positions.get(v).unwrap().1)
            .max()
            .unwrap();
        let shear = *max_y as f32 / (*max_x as f32 * 2f32);

        // coordinates
        for (v, (x, y)) in positions {
            let (x, y) = match options.slanted {
                true => (*x as f32 + shear * *y as f32, *y as f32 * 0.86602540378),
                false => (*x as f32, *y as f32),
            };

            let coord = if let Some(anchor) = &options.anchor {
                format!("($({}) + ({},{})$)", anchor, x, y)
            } else {
                format!("({},{})", x, y)
            };
            mid.extend(format!("{}\\coordinate (c_{}) at {} {{}};\n", ind, v.0, coord).chars());
        }

        mid.extend(format!("{}\n", ind).chars());

        // nodes
        for v in self.map.vertices() {
            let style = match v.weight {
                SchnyderVertexType::Normal(_) => "sws_normal_vertex",
                SchnyderVertexType::Suspension(SchnyderColor::Red) => "sws_red_suspension_vertex",
                SchnyderVertexType::Suspension(SchnyderColor::Green) => {
                    "sws_green_suspension_vertex"
                }
                SchnyderVertexType::Suspension(SchnyderColor::Blue) => "sws_blue_suspension_vertex",
            };
            mid.extend(
                format!(
                    "{}\\node[{}] ({}) at (c_{}) {{}};\n",
                    ind, style, v.id.0, v.id.0
                )
                .chars(),
            );
        }

        mid.extend(format!("{}\n", ind).chars());

        // midpoint coordinates
        for edge in self.map.edges() {
            let mid_point = format!("{}_{}", edge.tail.0, edge.head.0);
            let coord = format!(
                "{}\\coordinate ({}) at ($({})!0.5!({})$) {{}};\n",
                ind, mid_point, edge.tail.0, edge.head.0
            );
            mid.extend(coord.chars());
        }

        // edge heads (heads before tails in order to have a nice z-order next to the vertices)
        for edge in self.map.edges() {
            if let Unicolored(color, signum) = edge.weight {
                let mid_point = format!("{}_{}", edge.tail.0, edge.head.0);
                let end = match signum {
                    Signum::Forward => edge.head.0,
                    Signum::Backward => edge.tail.0,
                };

                mid.extend(
                    format!(
                        "{}\\draw[sws_edges_head, {}] ({}) -- ({});\n",
                        ind,
                        color_to_tikz(&color),
                        mid_point,
                        end
                    )
                    .chars(),
                );
            }
        }

        // edge tails
        for edge in self.map.edges() {
            if let Unicolored(color, signum) = edge.weight {
                let mid_point = format!("{}_{}", edge.tail.0, edge.head.0);
                let start = match signum {
                    Signum::Forward => edge.tail.0,
                    Signum::Backward => edge.head.0,
                };

                mid.extend(
                    format!(
                        "{}\\draw[sws_edges_tail, {}] ({}) -- ({});\n",
                        ind,
                        color_to_tikz(&color),
                        start,
                        mid_point
                    )
                    .chars(),
                );
            }
        }

        // bicolored edges
        for edge in self.map.edges() {
            if let Bicolored(fwd_c, bwd_c) = edge.weight {
                let mid_point = format!("{}_{}", edge.tail.0, edge.head.0);

                let str = if options.print_face_labels {
                    format!("{}\\draw[sws_bi_edges, {}] ({}) -- node[auto, inner sep=0pt] {{{}}} node[auto, swap, inner sep=0pt] {{{}}} ({});\n{}\\draw[sws_bi_edges, {}] ({}) -- ({});\n",
                            ind, color_to_tikz(&fwd_c), edge.tail.0, edge.left_face.unwrap().0,  edge.right_face.unwrap().0, mid_point, ind, color_to_tikz(&bwd_c), edge.head.0, mid_point)
                } else {
                    format!("{}\\draw[sws_bi_edges, {}] ({}) -- ({});\n{}\\draw[sws_bi_edges, {}] ({}) -- ({});\n",
                            ind, color_to_tikz(&fwd_c), edge.tail.0, mid_point, ind, color_to_tikz(&bwd_c), edge.head.0, mid_point)
                };

                mid.extend(str.chars());
            }
        }

        if let Some(title) = options.title {
            mid.extend(format!("{}\\node[yshift=-1.5em, align=center] at (current bounding box.south) {{{}}};\n", ind, title).chars());
        }

        let preamble = {
            if options.print_document {
                "\\documentclass[crop,tikz,border=10pt]{standalone}
\\begin{document}
\t\\usetikzlibrary{calc}
\t\\begin{tikzpicture}[x=10mm, y=10mm]\n"
            } else {
                "\\begin{tikzpicture}\n"
            }
        };

        let tail = if options.print_environment {
            if options.print_document {
                "\t\\end{tikzpicture}\n\\end{document}\n"
            } else {
                "\\end{tikzpicture}\n"
            }
        } else {
            ""
        };

        if options.print_environment {
            format!("{}{}{}", preamble, mid, tail)
        } else {
            mid
        }
    }
}
