use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::process;
use byteorder::{LittleEndian, WriteBytesExt};
use std::io::BufWriter;

fn write_triplet<W: Write>(writer: &mut W, first: u64, second: u64, third: u8) -> io::Result<()> {
    writer.write_u64::<LittleEndian>(first)?;
    writer.write_u64::<LittleEndian>(second)?;
    writer.write_u8(third)?;
    Ok(())
}

fn summarize_ranges(in_fname: &str, out_fname: &str) -> io::Result<()> {
    let infile = File::open(in_fname)?;
    let mut reader = BufReader::new(infile);
    let file = File::create(out_fname)?;
    let mut outfile = BufWriter::with_capacity(1 << 20, file); // 1 MB buffer

    // Write header triplets
    write_triplet(&mut outfile, 1094861636, 0, 0)?;
    write_triplet(&mut outfile, 0, 0, 0)?;
    write_triplet(&mut outfile, 0, 0, 0)?;

    let mut prev_first: Option<u64> = None;
    let mut prev_second: Option<u8> = None;
    let mut range_start: Option<u64> = None;

    let mut line = String::with_capacity(32); // Small preallocated buffer

    while reader.read_line(&mut line)? != 0 {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            line.clear();
            continue;
        }

        let mut parts = trimmed.split_whitespace();
        let Some(first_str) = parts.next() else {
            line.clear();
            continue;
        };
        let Some(second_str) = parts.next() else {
            line.clear();
            continue;
        };

        let Ok(first) = first_str.parse::<u64>() else {
            eprintln!("Skipping invalid number: {trimmed}");
            line.clear();
            continue;
        };
        let Ok(second) = second_str.parse::<u8>() else {
            eprintln!("Skipping invalid value: {trimmed}");
            line.clear();
            continue;
        };

        match (prev_first, prev_second, range_start) {
            (None, _, _) => {
                // First valid line
                prev_first = Some(first);
                prev_second = Some(second);
                range_start = Some(first);
            }
            (Some(pf), Some(ps), Some(rs)) => {
                if first == pf + 1 && second == ps {
                    // Extend current range
                    prev_first = Some(first);
                } else {
                    // Write completed range
                    write_triplet(&mut outfile, rs, pf, ps)?;
                    // Start new range
                    prev_first = Some(first);
                    prev_second = Some(second);
                    range_start = Some(first);
                }
            }
            _ => unreachable!(),
        }

        line.clear(); // REUSE buffer
    }

    // Write final range
    if let (Some(pf), Some(ps), Some(rs)) = (prev_first, prev_second, range_start) {
        write_triplet(&mut outfile, rs, pf, ps)?;
    }

    Ok(())
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: {} infile outfile", args[0]);
        process::exit(1);
    }

    if let Err(e) = summarize_ranges(&args[1], &args[2]) {
        eprintln!("Error: {}", e);
        process::exit(1);
    }
}

