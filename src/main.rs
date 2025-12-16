use serde::{Serialize, Deserialize};
use std::any::Any;
use std::thread::{self, JoinHandle, sleep};
use std::time::Duration;
use serde_json::{json, Value};
use rand::Rng;
use std::sync::mpsc;

#[derive(Serialize, Debug)]
struct IncomingPacket {
    op: i32,
    d: Value,
    s: Option<i32>,
    t: Option<String>,
}
#[derive(Serialize, Debug)]
struct OutgoingPacket<T> {
    op: i32,
    d: T,
}

fn main() {
    // Testing
    let mut rng = rand::rng();

    let (tx, rx) = mpsc::channel::<Option<i32>>();

    let mut hb: u64;
    let mut packet: IncomingPacket;

}

fn start_heartbeat(hb_duration: u64) -> JoinHandle<()> {
    thread::spawn(move || {
        let running = true;
        while running {
            let packet: OutgoingPacket<Option<i32>> = OutgoingPacket { op: 1, d: };
            println!("heartbeat: {:?}", packet);
            thread::sleep(Duration::from_millis(hb_duration));
        }
    })
}

fn stop_heartbeat(thread_handle: JoinHandle<()>) {
    thread_handle.join().unwrap();
}

fn opcode_handler(packet: &IncomingPacket, tx: &mpsc::Sender<Option<i32>>) {
   tx.send(packet.s).unwrap();
}