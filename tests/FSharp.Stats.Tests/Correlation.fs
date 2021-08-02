module CorrelationTests
open System
open FSharp.Stats.Correlation
open Expecto

[<Tests>]
let kendallCorrelationTests =
    // tested with R Kendall(x,y) function
    testList "Correlation.Seq" [
        let xs = [|-0.5;-0.4 ;0.  ;0.7;0.65;0.9649|]
        let ys = [|-0.3;-0.25;-0.1;-0.46;0.103;0.409|]

        testCase "kendall" <| fun () ->
            let tau = Seq.kendall xs ys
            Expect.floatClose Accuracy.high tau 0.4666666667 "Should be equal (double precision)"
    //ToDo ties tau_a,tau_b,tau_c

        testCase "kendallOfPairs" <| fun() ->
            let testCase1 = (xs, ys) ||> Seq.zip |> Seq.kendallOfPairs
            Expect.floatClose Accuracy.high testCase1 0.4666666667 "Should be equal (double precision)"

        testCase "kendallBy" <| fun() ->
            let testCase2 = 
                (xs, ys)
                ||> Seq.zip
                |> Seq.map (fun (x,y) -> {| xs = x; ys = y |})
                |> Seq.kendallBy (fun x -> x.xs, x.ys)
            Expect.floatClose Accuracy.high testCase2 0.4666666667 "Should be equal (double precision)"
    ]

[<Tests>]
let pearsonCorrelationTests =
    // examples from R
    // cor(x,y)
    let testCase1 =
        let seq1 = [44.4; 45.9; 41.9; 53.3; 44.7; 44.1; 50.7; 45.2; 60.1;]
        let seq2 = [ 2.6;  3.1;  2.5;  5.0;  3.6;  4.0;  5.2;  2.8;  3.8;]
        Seq.pearson seq1 seq2

    let testCase2 =
        let seq1 = [312.7; 104.2; 104.; 34.7]
        let seq2 = [315.5; 101.3; 108.; 32.2]
        Seq.pearson seq1 seq2

    let testCase3 = 
        [312.7, 315.5; 104.2, 101.3; 104., 108.; 34.7, 32.2]
        |> Seq.pearsonOfPairs

    let testCase4 = 
        [ {| A = 312.7; B = 315.5 |}
          {| A = 104.2; B = 101.3 |}
          {| A = 104.; B = 108. |}
          {| A = 34.7; B = 32.2 |} ]
        |> Seq.pearsonBy(fun x -> x.A, x.B)

    testList "Correlation.Seq" [
        testCase "pearson" <| fun () -> 
            Expect.isTrue (0.571181558 = Math.Round(testCase1,9)) "pearson correlation coefficient should be equal"
            Expect.isTrue (0.999705373 = Math.Round(testCase2,9)) "pearson correlation coefficient should be equal"
        testCase "pearsonOfPairs" <| fun () -> 
            Expect.isTrue (0.999705373 = Math.Round(testCase3,9)) "pearson correlation coefficient should be equal"
        testCase "pearsonBy" <| fun () -> 
            Expect.isTrue (0.999705373 = Math.Round(testCase4,9)) "pearson correlation coefficient should be equal"
     ]

