module LazyTests

open System.Threading
open NUnit.Framework
open Lazy
open FsUnit
let currentSecond() = System.DateTime.Now.Second
let naive = LazyFactory.NaiveLazy(currentSecond)
let lockFree = LazyFactory.LockFreeLazy(currentSecond)
let concurrent = LazyFactory.ConcurrentLazy(currentSecond)

let fst = naive.Get()
Thread.Sleep(1000)
let snd = naive.Get()
Thread.Sleep(1000)
let thrd = lockFree.Get()
Thread.Sleep(1000)
let frth = lockFree.Get()
Thread.Sleep(1000)
let ffth = concurrent.Get()
Thread.Sleep(1000)
let sxth = concurrent.Get()
Thread.Sleep(1000)
    

[<Test>]
let CheckThatLazyReturnsExpectedString() =
    let string = "idu po raionu jinsi visyat nizko"
    let foo = fun _ -> string
    let naiveChecker = LazyFactory.NaiveLazy(foo)
    let lockFreeChecker = LazyFactory.LockFreeLazy(foo)
    let concurrentChecker = LazyFactory.ConcurrentLazy(foo)
    
    naiveChecker.Get() |> should equal string
    lockFreeChecker.Get() |> should equal string
    concurrentChecker.Get() |> should equal string 
       
[<Test>]    
let CheckThatLazyAlwaysReturnsFirstCalculation() =   
    fst |> should equal snd
    thrd |> should equal frth
    ffth |> should equal sxth
    
[<Test>]   
let CheckConcurrency() =
    [ for _ in 0 .. 10 do async {
        Thread.Sleep(500)
        concurrent.Get() |> should equal ffth
        lockFree.Get() |> should equal thrd
    } ] |> Async.Parallel |> Async.RunSynchronously |> ignore
  