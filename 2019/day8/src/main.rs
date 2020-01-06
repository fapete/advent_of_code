struct ImageLayer {
    content: Vec<u32>,
}

impl ImageLayer {
    fn from(layer: &str) -> ImageLayer {
        let num_characters = layer.chars().count();
        if num_characters != 25 * 6 {
            panic!(
                "Image Layer needs to consist of {} characters, but got {}.",
                25 * 6,
                num_characters
            );
        }
        let content = layer
            .chars()
            .map(|x| x.to_digit(10).unwrap())
            .collect::<Vec<_>>();
        ImageLayer { content }
    }

    fn new() -> ImageLayer {
        ImageLayer {
            content: [2; 25 * 6].to_vec(),
        }
    }

    fn count(&self, digit: u32) -> usize {
        self.content.iter().filter(|x| **x == digit).count()
    }

    fn pretty_print_layer(&self) {
        for i in 0..6 {
            for j in &self.content[(i * 25)..((i + 1) * 25)] {
                match j {
                    0 => print!(" "),
                    1 => print!("*"),
                    2 => print!("_"),
                    _ => panic!("invalid colour code!"),
                }
            }
            print!("\n");
        }
    }

    fn part1(&self) -> usize {
        self.count(1) * self.count(2)
    }
}

fn merge_layers(image: Vec<ImageLayer>) -> ImageLayer {
    let mut merged_layer = ImageLayer::new();
    for layer in image {
        for i in 0..merged_layer.content.len() {
            if merged_layer.content[i] == 2 {
                merged_layer.content[i] = layer.content[i];
            }
        }
    }
    merged_layer
}

fn main() {
    let file = String::from(include_str!("../input"));
    let layer_length = 25 * 6;
    let layers_in_file = file.len() / layer_length;

    let mut image: Vec<ImageLayer> = Vec::new();
    for i in 0..layers_in_file {
        image.push(ImageLayer::from(
            &file[i * layer_length..(i + 1) * layer_length],
        ));
    }

    println!(
        "Ones * Twos in layer with least zeros: {}",
        image
            .iter()
            .min_by(|x, y| x.count(0).cmp(&y.count(0)))
            .unwrap()
            .part1()
    );

    // part 2:
    merge_layers(image).pretty_print_layer();
}
