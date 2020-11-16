use crate::schnyder::{SchnyderMap, SchnyderColor};
use crate::schnyder::SchnyderEdgeDirection::{Unicolored, Bicolored};
use crate::graph::enums::Signum;
use std::io::Write;

fn color_to_tikz(color: &SchnyderColor) -> &str {
    match color {
        SchnyderColor::Red => "red",
        SchnyderColor::Green => "green",
        SchnyderColor::Blue => "blue"
    }
}

impl SchnyderMap {

    pub fn write_tikz(&self, writer: &mut dyn Write, title: Option<&str>) -> std::io::Result<()> {
        let tikz_str = self.generate_tikz(title, false);
        writer.write_all(tikz_str.as_bytes())
    }

    pub fn generate_tikz(&self, title: Option<&str>, face_labels: bool) -> String {

        let face_counts = self.calculate_face_counts();

        let preamble = "\\documentclass[crop,tikz,border=10pt]{standalone}\\begin{document}\\tikzset{>=latex}\\usetikzlibrary{calc}\\begin{tikzpicture}[x=10mm, y=10mm]";
        let tail = "\\end{tikzpicture}\\end{document}";
        let mut mid = String::new();

        mid.push_str("\\tikzstyle{edges}=[->, shorten >= 2pt, thick]");
        mid.push_str("\\tikzstyle{biedges}=[->, thick]");
        //mid.push_str("\\tikzstyle{edges}=[line width=5pt, shorten >= 12pt]");
        //mid.push_str("\\tikzstyle{biedges}=[line width=5pt, shorten >= 1pt]");

        let max_red = self.map.vertex_indices().map(|v| face_counts.get(v).unwrap().0).max().unwrap();
        let max_green = self.map.vertex_indices().map(|v| face_counts.get(v).unwrap().1).max().unwrap();
        let shear = max_red as f32 / (max_green as f32 * 2f32);


        for v in self.map.vertices() {
            let (r, g, _) = face_counts.get(&v.id).expect(&format!("No face counts for vertex {} given", v.id.0));

            mid.extend(format!("\\coordinate ({}) at ({},{});", v.id.0, *g as f32 + shear * *r as f32, *r as f32 * 0.86602540378).chars());
            //mid.extend(format!("\\node at ({}) [above] {{${}$}};", v.id.0, v.id.0).chars());
        }

        for edge in self.map.edges() {
            mid.extend(match edge.weight {
                Unicolored(color, signum) => match signum {
                    Signum::Forward => if face_labels {
                        format!("\\draw[edges, {}] ({}) -- node[auto, inner sep=0pt] {{{}}} node[auto, swap, inner sep=0pt] {{{}}} ({});",  color_to_tikz(&color), edge.tail.0,  edge.left_face.unwrap().0,  edge.right_face.unwrap().0, edge.head.0)
                    } else {
                        format!("\\draw[edges, {}] ({}) -- ({});", color_to_tikz(&color), edge.tail.0, edge.head.0)
                    },
                    Signum::Backward => if face_labels {
                        format!("\\draw[edges, {}] ({}) --  node[auto, inner sep=0pt] {{{}}} node[auto, swap, inner sep=0pt] {{{}}} ({});",  color_to_tikz(&color), edge.head.0, edge.right_face.unwrap().0, edge.left_face.unwrap().0, edge.tail.0)
                    } else {
                        format!("\\draw[edges, {}] ({}) -- ({});",  color_to_tikz(&color), edge.head.0, edge.tail.0)
                    }
                }
                Bicolored(fwd_c, bwd_c) => {
                    let mid_point = format!("{}_{}", edge.tail.0, edge.head.0);

                    if face_labels {
                        format!("\\coordinate ({}) at ($({})!0.5!({})$) {{}};\\draw[biedges, {}] ({}) -- node[auto, inner sep=0pt] {{{}}} node[auto, swap, inner sep=0pt] {{{}}} ({});\\draw[biedges, {}] ({}) -- ({});",
                                mid_point, edge.tail.0, edge.head.0,  color_to_tikz(&fwd_c), edge.tail.0, edge.left_face.unwrap().0,  edge.right_face.unwrap().0, mid_point, color_to_tikz(&bwd_c), edge.head.0, mid_point)
                    } else {
                        format!("\\coordinate ({}) at ($({})!0.5!({})$) {{}};\\draw[biedges, {}] ({}) -- ({});\\draw[biedges, {}] ({}) -- ({});",
                                mid_point, edge.tail.0, edge.head.0,color_to_tikz(&fwd_c), edge.tail.0, mid_point, color_to_tikz(&bwd_c), edge.head.0, mid_point)
                    }
                }
            }.chars());
        }

        for v in self.map.vertices() {
            //let (_, _, _) = face_counts.get(&v.id).expect(&format!("No face counts for vertex {} given", v.id.0));
            mid.extend(format!("\\fill ({}) circle (2pt);", v.id.0).chars());
        }

        mid.extend(format!("\\draw[{},fill={}] ({}) circle (1pt);", color_to_tikz(&SchnyderColor::Red),  color_to_tikz(&SchnyderColor::Red), self.red_vertex.0).chars());
        mid.extend(format!("\\draw[{},fill={}] ({}) circle (1pt);", color_to_tikz(&SchnyderColor::Green),  color_to_tikz(&SchnyderColor::Green), self.green_vertex.0).chars());
        mid.extend(format!("\\draw[{},fill={}] ({}) circle (1pt);", color_to_tikz(&SchnyderColor::Blue),  color_to_tikz(&SchnyderColor::Blue), self.blue_vertex.0).chars());

        if let Some(title) = title {
            mid.extend(format!("\\node[yshift=2em, align=center, font=\\large\\bfseries] at (current bounding box.north) {{{}}};", title).chars());
        }

        return format!("{}{}{}", preamble, mid, tail);
    }

}