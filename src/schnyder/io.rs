use crate::schnyder::{SchnyderMap, SchnyderColor, SchnyderVertexType};
use crate::schnyder::SchnyderEdgeDirection::{Unicolored, Bicolored};
use crate::graph::enums::Signum;
use std::io::Write;

fn color_to_tikz(color: &SchnyderColor) -> &str {
    match color {
        SchnyderColor::Red => "sws_red",
        SchnyderColor::Green => "sws_green",
        SchnyderColor::Blue => "sws_blue"
    }
}

impl SchnyderMap {

    pub fn write_tikz(&self, writer: &mut dyn Write, title: Option<&str>, print_env: bool, print_styles: bool, anchor: Option<&str>) -> std::io::Result<()> {
        let tikz_str = self.generate_tikz(title, print_env, print_styles, false, anchor);
        writer.write_all(tikz_str.as_bytes())
    }

    pub fn generate_tikz(&self, title: Option<&str>, print_env: bool, print_styles: bool, print_face_labels: bool, anchor: Option<&str>) -> String {

        let face_counts = self.calculate_face_counts();

        let preamble = "\\documentclass[crop,tikz,border=10pt]{standalone}\n\\begin{document}\n\\usetikzlibrary{calc}\n\\begin{tikzpicture}[x=10mm, y=10mm]\n";
        let tail = "\\end{tikzpicture}\n\\end{document}\n";

        let mut mid = String::new();

        if print_styles {
            mid.push_str("\\tikzset {\n");
            mid.push_str("\tsws_edges_tail/.style = {},\n");
            mid.push_str("\tsws_edges_head/.style = {-latex},\n");
            mid.push_str("\tsws_bi_edges/.style = {-latex},\n");
            mid.push_str("\tsws_red/.style = {color=red},\n");
            mid.push_str("\tsws_green/.style = {color=green!80!black},\n");
            mid.push_str("\tsws_blue/.style = {color=blue},\n");
            mid.push_str("\tsws_red_suspension_vertex/.style = {circle, fill=black, scale=0.2},\n");
            mid.push_str("\tsws_green_suspension_vertex/.style = {circle, fill=black, scale=0.2},\n");
            mid.push_str("\tsws_blue_suspension_vertex/.style = {circle, fill=black, scale=0.2},\n");
            mid.push_str("\tsws_normal_vertex/.style = {circle, fill=black, scale=0.2},\n");
            mid.push_str("}\n");
        }

        let max_red = self.map.vertex_indices().map(|v| face_counts.get(v).unwrap().0).max().unwrap();
        let max_green = self.map.vertex_indices().map(|v| face_counts.get(v).unwrap().1).max().unwrap();
        let shear = max_red as f32 / (max_green as f32 * 2f32);

        for v in self.map.vertices() {
            let (r, g, _) = face_counts.get(&v.id).expect(&format!("No face counts for vertex {} given", v.id.0));

            let style = match v.weight {
                SchnyderVertexType::Normal(_) => "sws_normal_vertex",
                SchnyderVertexType::Suspension(SchnyderColor::Red) => "sws_red_suspension_vertex",
                SchnyderVertexType::Suspension(SchnyderColor::Green) => "sws_green_suspension_vertex",
                SchnyderVertexType::Suspension(SchnyderColor::Blue) => "sws_blue_suspension_vertex"
            };

            let (x, y) = (*g as f32 + shear * *r as f32, *r as f32 * 0.86602540378);

            let coord = if let Some(anchor) = anchor {
                format!("($({}) + ({},{})$)", anchor, x, y)
            } else {
                format!("({},{})", x, y)
            };
            mid.extend(format!("\\node[{}] ({}) at {} {{}};\n", style, v.id.0, coord).chars());
        }

        mid.extend("\n".chars());

        // midpoint coordinates
        for edge in self.map.edges() {
            let mid_point = format!("{}_{}", edge.tail.0, edge.head.0);
            let coord = format!("\\coordinate ({}) at ($({})!0.5!({})$) {{}};\n", mid_point, edge.tail.0, edge.head.0);
            mid.extend(coord.chars());
        }

        // edge heads
        for edge in self.map.edges() {
            if let Unicolored(color, signum) = edge.weight {
                let mid_point = format!("{}_{}", edge.tail.0, edge.head.0);
                let end = match signum {
                    Signum::Forward => edge.head.0,
                    Signum::Backward => edge.tail.0
                };

                mid.extend(format!("\\draw[sws_edges_head, {}] ({}) -- ({});\n",
                        color_to_tikz(&color), mid_point, end).chars());
            }
        }

        // edge tails
        for edge in self.map.edges() {
            if let Unicolored(color, signum) = edge.weight {
                let mid_point = format!("{}_{}", edge.tail.0, edge.head.0);
                let start = match signum {
                    Signum::Forward => edge.tail.0,
                    Signum::Backward => edge.head.0
                };

                mid.extend(format!("\\draw[sws_edges_tail, {}] ({}) -- ({});\n",
                                   color_to_tikz(&color), start, mid_point).chars());
            }
        }

        // bicolored edges
        for edge in self.map.edges() {

            if let  Bicolored(fwd_c, bwd_c) = edge.weight {
                let mid_point = format!("{}_{}", edge.tail.0, edge.head.0);
                let coord = format!("\\coordinate ({}) at ($({})!0.5!({})$) {{}};\n", mid_point, edge.tail.0, edge.head.0);

                let str = if print_face_labels {
                    format!("{}\\draw[sws_bi_edges, {}] ({}) -- node[auto, inner sep=0pt] {{{}}} node[auto, swap, inner sep=0pt] {{{}}} ({});\n\\draw[sws_bi_edges, {}] ({}) -- ({});\n",
                            coord, color_to_tikz(&fwd_c), edge.tail.0, edge.left_face.unwrap().0,  edge.right_face.unwrap().0, mid_point, color_to_tikz(&bwd_c), edge.head.0, mid_point)
                } else {
                    format!("{}\\draw[sws_bi_edges, {}] ({}) -- ({});\n\\draw[sws_bi_edges, {}] ({}) -- ({});\n",
                            coord, color_to_tikz(&fwd_c), edge.tail.0, mid_point, color_to_tikz(&bwd_c), edge.head.0, mid_point)
                };

                mid.extend(str.chars());
            }
        }

        if let Some(title) = title {
            mid.extend(format!("\\node[yshift=2em, align=center, font=\\large\\bfseries] at (current bounding box.north) {{{}}};\n", title).chars());
        }

        if print_env {
            format!("{}\n{}\n{}", preamble, mid, tail)
        } else {
            mid
        }
    }

}