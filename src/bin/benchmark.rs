use std::time::Instant;

fn mandelbrot(width: usize, height: usize, iterations: usize) -> Vec<u32> {
    let mut pixels: Vec<u32> = Vec::with_capacity(width * height);
    for _ in 0..(width * height) {
        pixels.push(0);
    }

    for y in 0..height {
        for x in 0..width {
            let mut zx: f64 = 0.0;
            let mut zy: f64 = 0.0;
            let cx: f64 = (x as f64 / width as f64) * 3.5 - 2.5;
            let cy: f64 = (y as f64 / height as f64) * 2.0 - 1.0;
            let mut iter: usize = 0;

            while zx * zx + zy * zy < 2.5 && iter < iterations {
                let tmp: f64 = zx * zx - zy * zy + cx;
                zy = 2.0 * zx * zy + cy;
                zx = tmp;
                iter += 1;
            }

            pixels[y * width + x] = iter as u32;
        }
    }

    return pixels;
}

fn main() {
    let iter: usize = 200;
    let size: usize = 512;
    let start = Instant::now();

    for _ in 0..iter {
        mandelbrot(size, size, 60);
    }

    println!("{:?}", start.elapsed() / iter.try_into().unwrap());
}
