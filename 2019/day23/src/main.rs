use day23::intcode_computer::{Machine, Packet};
use std::sync::mpsc;
use std::thread::JoinHandle;
use std::{thread, time};

struct NAT {
    nic_handles: Vec<JoinHandle<()>>,
    nic_inputs: Vec<mpsc::Sender<Packet>>,
    packet_queue: mpsc::Receiver<Packet>,
    last_packet: Option<Packet>,
    last_y: isize,
}

impl NAT {
    fn new() -> NAT {
        let mut nic_handles = Vec::new();
        let mut nic_inputs = Vec::new();
        let (mytx, myrx) = mpsc::channel();
        for i in 0..50 {
            let nic = get_computer(mpsc::Sender::clone(&mytx), i as isize);
            let (nic_handle, nic_tx) = nic.run();
            nic_handles.push(nic_handle);
            nic_inputs.push(nic_tx);
        }
        NAT {
            nic_handles,
            nic_inputs,
            packet_queue: myrx,
            last_packet: None,
            last_y: -1,
        }
    }

    fn run(&mut self) {
        let mut empty_queue_trys = 0;
        loop {
            match self.packet_queue.try_recv() {
                Ok(rec_packet) => {
                    empty_queue_trys = 0;
                    let add = rec_packet.get_receiver();
                    if add == 255 {
                        if self.last_packet.is_none() {
                            println!("First Packet with Address 255: {:?}", rec_packet);
                        }
                        self.receive_nat_packet(rec_packet);
                    } else {
                        self.nic_inputs[add].send(rec_packet).unwrap();
                    }
                }
                Err(_) => {
                    // Wait for a bit to let the NICs work, but if we don't get something three
                    // times in a row, consider network idle.
                    thread::sleep(time::Duration::from_millis(50));
                    empty_queue_trys += 1;
                    if empty_queue_trys == 3 && self.last_packet.is_some() {
                        self.nic_inputs[0].send(self.last_packet.unwrap()).unwrap();
                        if self.last_packet.unwrap().read_y() == self.last_y {
                            println!(
                                "Delivered a packet with y-value {} twice in a row.",
                                self.last_y
                            );
                        }
                        self.last_y = self.last_packet.unwrap().read_y();
                        empty_queue_trys = 0;
                    }
                }
            }
        }
    }

    fn receive_nat_packet(&mut self, p: Packet) {
        self.last_packet = Some(p);
    }

    fn finalize(self) {
        for handle in self.nic_handles {
            handle.join().unwrap();
        }
    }
}

fn main() {
    let mut net = NAT::new();
    net.run();
    net.finalize();
}

fn get_computer(tx: mpsc::Sender<Packet>, address: isize) -> Machine {
    let tape = include_str!("../input");
    Machine::from(tape, tx, address)
}
