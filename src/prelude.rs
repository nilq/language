use textplots::{utils, Chart, Plot, Shape};

use rand_distr::{Normal};

use rand::distributions::{Distribution, Uniform};
use rand::prelude::*;

use rand::seq::index::sample;
use rand::seq::SliceRandom;

use histogram::Histogram;

use crate::vm::{heap::Heap, value::{Value, object::{Obj, List}, Variant}};

pub fn assert(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if !args[1].truthy() {
        panic!("assertion failed.")
    }

    Value::truelit()
}

pub fn gc(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    heap.clean();
    Value::nil()
}

pub fn gaussian(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if let Variant::Number(mean) = args[1].decode() {
        if let Variant::Number(std_dev) = args[2].decode() {
            let normal = Normal::new(mean, std_dev).unwrap();

            let v = normal.sample(&mut rand::thread_rng());

            Value::number(v)
        } else {
            panic!("no std-dev")
        }
    } else {
        panic!("no mean")
    }
}

pub fn uniform(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if let Variant::Number(a) = args[1].decode() {
        if let Variant::Number(b) = args[2].decode() {
            let normal = Uniform::new(a, b);

            let v = normal.sample(&mut rand::thread_rng());

            Value::number(v)
        } else {
            panic!("no high")
        }
    } else {
        panic!("no low")
    }
}

pub fn zeros(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if let Variant::Number(len) = args[1].decode() {
        let list = Obj::List(
            List::new(
                vec![0i32;len as usize].iter().collect::<Vec<&i32>>()
                    .iter()
                    .map(|x| Value::number(**x as f64))
                    .collect::<Vec<Value>>()
            )
        );
        let handle = heap.insert(list).into_handle();

        return Value::object(handle)
    } else {
        panic!("len must be number.")
    }
}

pub fn range(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if let Variant::Number(len) = args[1].decode() {
        let list = Obj::List(
            List::new(
                (0..len as usize).collect::<Vec<usize>>()
                    .iter()
                    .map(|x| Value::number(*x as f64))
                    .collect::<Vec<Value>>()
            )
        );
        let handle = heap.insert(list).into_handle();

        return Value::object(handle)
    } else {
        panic!("len must be number.")
    }
}

pub fn len(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if let Variant::Obj(handle) = args[1].decode() {
        let list = unsafe { heap.get_unchecked(handle) };

        if let Some(ref mut list) = list.as_list() {            
            return Value::number(list.content.len() as f64)
        } else {
            panic!("can't get length of non-list")
        }
    } else {
        panic!("can't get length of non-list")
    }
}



pub fn append(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if let Variant::Obj(ref mut handle) = args[1].decode() {
        let list = unsafe { heap.get_unchecked(handle.clone()) };

        if let Some(ref mut list_list) = list.as_list() {      
            let mut content = list_list.content.clone();
            content.push(args[2]);

            let obj = Obj::List(List::new(content));
            let list_handle = heap.insert(obj).into_handle();

            Value::object(list_handle)
        } else {
            panic!("can't get length of non-list")
        }
    } else {
        panic!("can't get length of non-list")
    }
}


pub fn sum(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if let Variant::Obj(handle) = args[1].decode() {
        let list = unsafe { heap.get_unchecked(handle) };

        if let Some(list) = list.as_list() {
            let mut sum = 0f64;

            for item in list.content.iter() {
                if let Variant::Number(n) = item.decode() {
                    sum += n
                } else {
                    panic!("can't sum non-float")
                }
            }

            Value::number(sum)
        } else {
            panic!("can't sum non-list")
        }
    } else {
        let mut sum = 0f64;
        for arg in args {
            if let Variant::Number(n) = arg.decode() {
                sum += n
            } else {
                panic!("can't sum non-float")
            }
        }

        Value::number(sum)
    }
}

pub fn choose(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if let Variant::Obj(handle) = args[1].decode() {
        let list = unsafe { heap.get_unchecked(handle) };

        if let Some(list) = list.as_list() {
            let mut rng = thread_rng();

            list.content.choose(&mut rng).unwrap().clone()
        } else {
            panic!("can't sum non-list")
        }
    } else {
        let mut sum = 0f64;
        for arg in args {
            if let Variant::Number(n) = arg.decode() {
                sum += n
            } else {
                panic!("can't sum non-float")
            }
        }

        Value::number(sum)
    }
}

pub fn shuffle(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if let Variant::Obj(handle) = args[1].decode() {
        let list = unsafe { heap.get_unchecked(handle) };

        if let Some(ref mut list) = list.as_list() {
            let mut content = list.content.clone();
            let mut rng = rand::thread_rng();

            content.shuffle(&mut rng);

            let list = Obj::List(List::new(content));
            let handle = heap.insert(list).into_handle();

            return Value::object(handle)
        } else {
            panic!("can't shuffle non-list")
        }
    } else {
        panic!("can't shuffle non-list")
    }
}

pub fn random(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    let mut rng = rand::thread_rng();
    let y: f64 = rng.gen();

    Value::number(y)
}

pub fn random_sample(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    let mut rng = rand::thread_rng();
    
    if let Variant::Number(length) = args[1].decode() {
        if let Variant::Number(n) = args[2].decode() {
            let sample = sample(&mut rng, length as usize, n as usize).iter().collect::<Vec<usize>>();

            let list = Obj::List(
                List::new(
                    sample
                        .iter()
                        .map(|x| Value::number(*x as f64))
                        .collect::<Vec<Value>>()
                )
            );
            let handle = heap.insert(list).into_handle();
    
            return Value::object(handle)
            
        } else {
            panic!("can't sample without `n`")
        }
    } else {
        panic!("can't sample without `length`")
    }
}

pub fn plot(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if let Variant::Obj(handle) = args[1].decode() {
        let name = unsafe { heap.get_unchecked(handle) }.as_string().unwrap();
        println!("Plot: {}", name);

        if let Variant::Obj(handle_x) = args[2].decode() {
            if let Variant::Obj(handle_y) = args[3].decode() {
                let list_x = unsafe { heap.get_unchecked(handle_x) };
                let list_y = unsafe { heap.get_unchecked(handle_y) };


                if let Some(ref mut list_x) = list_x.as_list() {
                    if let Some(ref mut list_y) = list_y.as_list() {

                        let xs = list_x.content
                            .iter()
                            .map(|x| if let Variant::Number(x) = x.decode() { x } else { panic!("can't plot non-number") })
                            .collect::<Vec<f64>>();

                        let ys = list_y.content
                            .iter()
                            .map(|y| if let Variant::Number(y) = y.decode() { y } else { panic!("can't plot non-number") })
                            .collect::<Vec<f64>>();
                        
                        let points = xs.iter().zip(&ys)
                            .map(|(x, y)| (*x as f32, *y as f32))
                            .collect::<Vec<(f32, f32)>>();

                        Chart::new(xs[xs.len() - 1] as u32 / 10 * 32, 48, xs[0] as f32 - 1.0, xs[xs.len() - 1] as f32 + 1.0).lineplot(&Shape::Lines(&points.as_slice())).display();

                        return Value::nil()
                    }
                }
            }
        }
    }

    panic!("can't plot non-list")
}


pub fn histogram(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if let Variant::Obj(handle) = args[1].decode() {
        let name = unsafe { heap.get_unchecked(handle) }.as_string().unwrap();
        println!("Plot: {}", name);

        if let Variant::Obj(handle_x) = args[2].decode() {
            if let Variant::Obj(handle_y) = args[3].decode() {
                let list_x = unsafe { heap.get_unchecked(handle_x) };
                let list_y = unsafe { heap.get_unchecked(handle_y) };


                if let Some(ref mut list_x) = list_x.as_list() {
                    if let Some(ref mut list_y) = list_y.as_list() {

                        let xs = list_x.content
                            .iter()
                            .map(|x| if let Variant::Number(x) = x.decode() { x } else { panic!("can't plot non-number") })
                            .collect::<Vec<f64>>();

                        let ys = list_y.content
                            .iter()
                            .map(|y| if let Variant::Number(y) = y.decode() { y } else { panic!("can't plot non-number") })
                            .collect::<Vec<f64>>();
                        
                        let points = xs.iter().zip(&ys)
                            .map(|(x, y)| (*x as f32, *y as f32))
                            .collect::<Vec<(f32, f32)>>();

                        let mut histo = Histogram::new();

                        for (x, y) in points.iter() {
                            histo.increment(*y as u64).unwrap();
                        }

                        println!("Percientiler: Min: {} | Mean: {} | Max: {} | Std. Dev.: {}",
                            histo.minimum().unwrap(),
                            histo.mean().unwrap(),
                            histo.maximum().unwrap(),
                            histo.stddev().unwrap(),
                        );

                        if let Variant::Number(bins) = args[4].decode() {
                            let min = histo.minimum().unwrap() as f32;
                            let max = (histo.maximum().unwrap() + 10) as f32;

                            let hist = utils::histogram(&points.as_slice(), min, max, bins as usize);

                            Chart::new(180, 60, min, max)
                                .lineplot(&Shape::Bars(&hist)).display();

                            return Value::nil()
                        }
                    }
                }
            }
        }
    }

    panic!("can't plot non-list")
}

pub fn sorted(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if let Variant::Obj(handle) = args[1].decode() {
        let list = unsafe { heap.get_unchecked(handle) };

        if let Some(list) = list.as_list() {
            let mut sorted = Vec::new();

            for item in list.content.iter() {
                if let Variant::Number(n) = item.decode() {
                    sorted.push(n)
                } else {
                    panic!("can't sort non-float")
                }
            }

            sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());

            let list = Obj::List(
                List::new(
                    sorted
                        .iter()
                        .map(|x| Value::number(*x as f64))
                        .collect::<Vec<Value>>()
                )
            );
            let handle = heap.insert(list).into_handle();
    
            return Value::object(handle)
        } else {
            panic!("can't sort non-list")
        }
    } else { 
        panic!("can't sort non-list")
    }
}
