module LazyTests

open NUnit.Framework
open Lazy
open FsUnit

[<Test>]
let NaiveLazyShouldReturnExpectedString() =
    let string = "idu po raionu jinsi visyat nizko"
    let foo = fun _ -> string
    let naiveChecker = LazyFactory.NaiveLazy(foo)  
    naiveChecker.Get() |> should equal string
    
[<Test>]
let LockFreeLazyShouldReturnExpectedString() =
    let string = "beb"
    let foo = fun _ -> string
    let lockFreeChecker = LazyFactory.LockFreeLazy(foo)
    lockFreeChecker.Get() |> should equal string
    
[<Test>]
let ConcurrentLazyShouldReturnExpectedString() =
    let string = "opa gagnam style"
    let foo = fun _ -> string
    let concurrentChecker = LazyFactory.ConcurrentLazy(foo)
    concurrentChecker.Get() |> should equal string 
       
[<Test>]    
let CheckThatNaiveLazyAlwaysReturnsFirstCalculation() =
    let currentSecond() = System.DateTime.Now.Second 
    let naive = LazyFactory.NaiveLazy(currentSecond)
    naive.Get() |> should equal (naive.Get())
    
[<Test>]    
let CheckThatLockFreeLazyAlwaysReturnsFirstCalculation() =
    let currentSecond() = System.DateTime.Now.Second 
    let lockFree = LazyFactory.LockFreeLazy(currentSecond)
    lockFree.Get() |> should equal (lockFree.Get())
    
[<Test>]
let CheckThatConcurrentLazyAlwaysReturnsFirstCalculation() =
    let currentSecond() = System.DateTime.Now.Second 
    let concurrent = LazyFactory.ConcurrentLazy(currentSecond)  
    concurrent.Get() |> should equal (concurrent.Get())
    
[<Test>]   
let CheckConcurrencyInConcurrentLazy() =
    let random() = System.Random().Next()
    let concurrent = LazyFactory.ConcurrentLazy(random)
    [ for _ in 0 .. 100 do async {
        concurrent.Get() |> should equal (concurrent.Get())
    } ] |> Async.Parallel |> Async.RunSynchronously |> ignore
    
[<Test>]   
let CheckConcurrencyInLockFreeLazy() =
    let random() = System.Random().Next()
    let lockFree = LazyFactory.LockFreeLazy(random)
    [ for _ in 0 .. 100 do async {
        lockFree.Get() |> should equal (lockFree.Get())
    } ] |> Async.Parallel |> Async.RunSynchronously |> ignore