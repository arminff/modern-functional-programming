import Head from 'next/head'
import Link from 'next/link'
import styles from '../../styles/Home.module.css'

export default function RustModule3() {
  return (
    <div className={styles.container}>
      <Head>
        <title>Rust Module 3: Advanced Applications | Modern Functional Programming</title>
        <meta name="description" content="Learn advanced applications and real-world projects in Rust" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <div className={styles.backLink}>
          <Link href="/rust">← Back to Rust</Link>
        </div>

        <div className={styles.moduleHeader}>
          <div className={styles.languageIcon}>🦀</div>
          <h1 className={styles.title}>Rust Module 3: Advanced Applications</h1>
          <p className={styles.description}>
            Real-world applications and advanced Rust projects
          </p>
        </div>

        <div className={styles.moduleContent}>
          <div className={styles.progressBar}>
            <div className={styles.progressFill} style={{ width: '75%' }}></div>
          </div>
          
          <div className={styles.moduleSection}>
            <h2>Web Development with Rust</h2>
            <p>Rust has excellent frameworks for building web applications with high performance and safety.</p>
            
            <h3>Actix Web</h3>
            <p>Actix Web is a powerful, pragmatic, and extremely fast web framework for Rust:</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`use actix_web::{web, App, HttpResponse, HttpServer};

async fn hello() -> HttpResponse {
    HttpResponse::Ok().body("Hello, World!")
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
            .route("/", web::get().to(hello))
    })
    .bind("127.0.0.1:8080")?
    .run()
    .await
}`}
                </code>
              </pre>
            </div>

            <h3>Rocket</h3>
            <p>Rocket is a web framework for Rust that makes it simple to write fast, secure web applications:</p>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`#[macro_use] extern crate rocket;

#[get("/")]
fn index() -> &'static str {
    "Hello, world!"
}

#[launch]
fn rocket() -> _ {
    rocket::build().mount("/", routes![index])
}`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Systems Programming</h2>
            <p>Rust excels at systems programming, allowing you to write safe, efficient low-level code.</p>
            
            <h3>Memory Management</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`use std::alloc::{alloc, dealloc, Layout};

unsafe fn allocate_memory(size: usize) -> *mut u8 {
    let layout = Layout::from_size_align_unchecked(size, 1);
    alloc(layout)
}

unsafe fn free_memory(ptr: *mut u8, size: usize) {
    let layout = Layout::from_size_align_unchecked(size, 1);
    dealloc(ptr, layout);
}`}
                </code>
              </pre>
            </div>

            <h3>FFI (Foreign Function Interface)</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`#[no_mangle]
pub extern "C" fn rust_function(x: i32) -> i32 {
    x * 2
}`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Concurrent Applications</h2>
            <p>Building robust concurrent applications with Rust's safety guarantees.</p>
            
            <h3>Async/Await</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`use tokio;

async fn fetch_data() -> Result<String, Box<dyn std::error::Error>> {
    let response = reqwest::get("https://api.example.com/data")
        .await?
        .text()
        .await?;
    Ok(response)
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let data = fetch_data().await?;
    println!("{}", data);
    Ok(())
}`}
                </code>
              </pre>
            </div>

            <h3>Parallel Processing</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`use rayon::prelude::*;

fn process_data(data: &[i32]) -> Vec<i32> {
    data.par_iter()
        .map(|&x| x * x)
        .collect()
}`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Embedded Systems</h2>
            <p>Rust is increasingly used in embedded systems development for its safety and performance.</p>
            
            <h3>no_std Applications</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`#![no_std]
#![no_main]

use core::panic::PanicInfo;
use embedded_hal::digital::v2::OutputPin;

#[no_mangle]
pub extern "C" fn _start() -> ! {
    // Main application loop
    loop {
        // Blink LED
    }
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}`}
                </code>
              </pre>
            </div>

            <h3>RTIC (Real-Time Interrupt-driven Concurrency)</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`use rtic::cyccnt::U32Ext;
use stm32f4xx_hal::prelude::*;

#[rtic::app(device = stm32f4xx_hal::stm32::Peripherals)]
mod app {
    use super::*;

    #[shared]
    struct Shared {}

    #[local]
    struct Local {}

    #[init]
    fn init(cx: init::Context) -> (Shared, Local) {
        // Initialize hardware
        (Shared {}, Local {})
    }

    #[task]
    fn task1(cx: task1::Context) {
        // Task implementation
    }
}`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleSection}>
            <h2>Game Development</h2>
            <p>Rust has a growing ecosystem for game development with frameworks like Amethyst and Bevy.</p>
            
            <h3>Bevy ECS</h3>
            <div className={styles.codeBlock}>
              <pre>
                <code>
                  {`use bevy::prelude::*;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_systems(Startup, setup)
        .add_systems(Update, movement)
        .run();
}

fn setup(mut commands: Commands) {
    commands.spawn(Camera2dBundle::default());
    commands.spawn(SpriteBundle {
        sprite: Sprite {
            color: Color::rgb(0.25, 0.25, 0.75),
            custom_size: Some(Vec2::new(50.0, 50.0)),
            ..default()
        },
        transform: Transform::from_xyz(0.0, 0.0, 0.0),
        ..default()
    });
}

fn movement(
    time: Res<Time>,
    mut query: Query<&mut Transform, With<Sprite>>,
) {
    for mut transform in &mut query {
        transform.translation.x += 100.0 * time.delta_seconds();
    }
}`}
                </code>
              </pre>
            </div>
          </div>

          <div className={styles.moduleResources}>
            <h2>Additional Resources</h2>
            <ul>
              <li>
                <a href="https://actix.rs/" target="_blank" rel="noopener noreferrer">
                  Actix Web Documentation
                </a>
              </li>
              <li>
                <a href="https://rocket.rs/" target="_blank" rel="noopener noreferrer">
                  Rocket Web Framework
                </a>
              </li>
              <li>
                <a href="https://bevyengine.org/" target="_blank" rel="noopener noreferrer">
                  Bevy Game Engine
                </a>
              </li>
              <li>
                <a href="https://www.rust-lang.org/tools/embedded" target="_blank" rel="noopener noreferrer">
                  Rust Embedded Working Group
                </a>
              </li>
            </ul>
          </div>
        </div>

        <div className={styles.moduleNavigation}>
          <div className={styles.buttonGroup}>
            <a href="/rust/module2" className={styles.secondaryButton}>Previous Module: Advanced Concepts</a>
            <a href="/rust" className={styles.primaryButton}>Back to Rust</a>
          </div>
        </div>
      </main>

      <footer className={styles.footer}>
        <p>© 2023 Modern Functional Programming. All rights reserved.</p>
      </footer>
    </div>
  )
} 