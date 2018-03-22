let prodOfNum n =
    let rec prodNum num res =
        if num < 1 then res else prodNum (num / 10) (res * (num % 10))
    prodNum (abs n / 10) ((abs n) % 10)

let findNum num list =
    let rec findNumRec num numInList list =
        match list with
        | [] -> None
        | a::_ when a = num -> Some(numInList)
        | _ -> findNumRec num (numInList + 1) (List.tail list)
    findNumRec num 1 list

let isPolindrom str =
    let rec isPolindromRec (str : string) left right =
        match (left, right) with
        | (a, b) when a >= b -> true
        | (a, b) when not (System.Byte.Equals(str.[left], str.[right])) -> false
        | (a, b) -> isPolindromRec str (left + 1) (right - 1)
    isPolindromRec str 0 (String.length str - 1)   

let mergeSort list = 
    let rec unionOfSortList list1 list2 res =
        match (list1, list2) with
        | ([], []) -> List.rev res
        | ([], _) -> unionOfSortList list1 (List.tail list2) ((List.head list2)::res)
        | (_, []) -> unionOfSortList (List.tail list1) list2 ((List.head list1)::res)
        | (_, _) when (List.head list1) < (List.head list2) -> unionOfSortList (List.tail list1) list2 ((List.head list1)::res)
        | (_, _) -> unionOfSortList list1 (List.tail list2) ((List.head list2)::res)
    
    let rec mergeSortRec list1 list2 =
        let l1 = List.length list1
        let l2 = List.length list2
        match (l1, l2) with
        | (a, b) when a < 1 || b < 1 -> list1 @ list2
        | (a, b) when a = 1 && b = 1 -> unionOfSortList list1 list2 []
        | (a, b) when a = 1 && b > 1 -> unionOfSortList list1 (mergeSortRec list2.[0..(l2 / 2) - 1] list2.[l2 / 2..(l2 - 1)]) []
        | (a, b) when a > 1 && b = 1 -> unionOfSortList (mergeSortRec list1.[0..(l1 / 2) - 1] list1.[l1 / 2..(l1 - 1)]) list2 []
        | (_, _) -> unionOfSortList (mergeSortRec list1.[0..(l1 / 2) - 1] list1.[l1 / 2..(l1 - 1)]) (mergeSortRec list2.[0..(l2 / 2) - 1] list2.[l2 / 2..(l2 - 1)]) []

    let len = List.length list

    if len < 2
    then list
    else mergeSortRec list.[0..(len / 2) - 1] list.[len / 2..(len - 1)]   

[<EntryPoint>]
let main argv =
    let res = mergeSort [1; 5; 3; 7; 9; 12; 15; 2]
    let res1 = mergeSort []
    let res2 = mergeSort [1]
    let res3 = mergeSort [1; 5]
    let res4 = mergeSort [14; 5; 3]
    let res5 = mergeSort [6; 5; 4; 3; 2; 1]

    let res6 = isPolindrom "a"
    let res7 = isPolindrom ""
    let res8 = isPolindrom "abba"
    let res9 = isPolindrom "abcd"
    let res10 = isPolindrom "arrrerrra"

    let res11 = findNum 4 [1; 2; 3; 4; 5]
    let res12 = findNum 2 [1; 1]
    let res13 = findNum 4 [1; 5]
    let res14 = findNum 5 []
    let res15 = findNum 6 [1; 6; 6; 7; 8]

    let res16 = prodOfNum 1234
    let res17 = prodOfNum 10005
    let res18 = prodOfNum -156
    let res19 = prodOfNum -5
    let res20 = prodOfNum 0

    0 
