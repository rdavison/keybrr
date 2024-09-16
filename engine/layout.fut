type char = i64
type string [n] = [n]char
type option 't = #none | #some(t)

let cartesian_product 't 'u (arr1: []t) (arr2: []u): [](t,u) =
    -- Flatten the result by creating pairs of every element from arr1 with every element from arr2
    (iota (length arr1))
    |> map (\i -> 
        (iota (length arr2))
        |> map (\j -> 
            (arr1[i], arr2[j]))) 
    |> flatten

module Corpus : {
    module Item : {
        type t [n] = {
            raw: [n][n]f32,
            raw_total: f32,
            norm: [n][n]f32
        }

        val make [n] : *[n][n]f32 -> *t[n]
    }

    type t [n] = {
        b0: Item.t[n],
        b1: Item.t[n]
    }

    val make [n]: *[n][n]f32 -> *[n][n]f32 -> *t[n]
    val b0 [n]: string[2] -> t[n] -> f32
    val b1 [n]: string[2] -> t[n] -> f32
} = {
    module Item = {
        type t [n] = {
            raw: [n][n]f32,
            raw_total: f32,
            norm: [n][n]f32
        }

        let make [n] (raw : *[n][n]f32): *t[n] =
            let raw_total = map f32.sum raw |> f32.sum
            let norm =
                map (\row ->
                    map (\x -> x f32./ raw_total) row)
                    raw
            in {raw, raw_total, norm}
    }

    type t [n] = {
        b0: Item.t[n],
        b1: Item.t[n]
    }

    let make [n] (b0 : *[n][n]f32) (b1 : *[n][n]f32): *t[n] =
        {
            b0 = Item.make b0,
            b1 = Item.make b1
        }

    let lookup [n] [m] (f : t[n] -> string[m] -> f32) (key: string[m]) (t : t[n]) = f t key
    let b0 [n] (key: string[2]) (t: t[n]) = lookup (\t key -> t.b0.norm[key[0]][key[1]]) key t
    let b1 [n] (key: string[2]) (t: t[n]) = lookup (\t key -> t.b1.norm[key[0]][key[1]]) key t
}

module Hand = {
    type t = #l | #r

    let all: []t = [#l, #r]
}

module Finger = {
    type t = #p | #r | #m | #i | #t

    let all: []t = [#p, #r, #m, #i, #t]
}

module Hf = {
    type t = (Hand.t, Finger.t)

    let all: []t = [
        (#l, #p),
        (#l, #r),
        (#l, #m),
        (#l, #i),
        (#l, #t),
        (#r, #t),
        (#r, #i),
        (#r, #m),
        (#r, #r),
        (#r, #p)
    ]

    let left_of (t: t) : option t =
        match t
            case (#l, #p) -> #none
            case (#l, #r) -> #some(#l, #p)
            case (#l, #m) -> #some(#l, #r )
            case (#l, #i) -> #some(#l, #m)
            case (#l, #t) -> #some(#l, #i)
            case (#r, #t) -> #none
            case (#r, #i) -> #some(#r, #t)
            case (#r, #m) -> #some(#r, #i)
            case (#r, #r) -> #some(#r, #m)
            case (#r, #p) -> #some(#r, #r)

    let right_of (t: t) : option t =
        match t
            case (#l, #p) -> #some(#l, #r)
            case (#l, #r) -> #some(#l, #m)
            case (#l, #m) -> #some(#l, #i)
            case (#l, #i) -> #some(#l, #t)
            case (#l, #t) -> #none
            case (#r, #t) -> #some(#r, #i)
            case (#r, #i) -> #some(#r, #m)
            case (#r, #m) -> #some(#r, #r)
            case (#r, #r) -> #some(#r, #p)
            case (#r, #p) -> #none
}

module Stroke = {
    type t = {
        hand: Hand.t,
        finger: Finger.t,
        row: i64,
        col: i64,
        index: i64
    }

    let empty: t = {
        hand = #l,
        finger = #p,
        row = -1,
        col = -1,
        index = -1
    }
}

module Layout = {
    type t [n] = {
        codes: [n]char,
        fingers: [n]Finger.t,
        hands: [n]Hand.t,
        rows: [n]i64,
        cols: [n]i64,
        num_fingers: i64
    }

    let keys_for_finger [n] (finger: Finger.t) (layout: t [n]) =
        zip layout.codes layout.fingers |> filter (\x -> x.1 == finger) |> map (.0)
}

let layout_to_strokes [n] (layout: Layout.t[n]) : [n] Stroke.t =
    tabulate n (\i ->
        {
            hand = layout.hands[i],
            finger = layout.fingers[i],
            row = layout.rows[i],
            col = layout.cols[i],
            index = i
        }
    )

module type Layout = {
    type t = Layout.t[31]

    val default: t
    val strokes: [31]Stroke.t
}

module Ansi31DAM = {
    type t = Layout.t[31]

    let default: t = 
        let codes = [
            'b','l','c','w','k','j','y','o','u','\'',
            'n','r','s','t','m','d','h','e','i','a',','
            ,'x','z','v','g','q','p','f','/','.',';'
        ]
        let hands = [
            #l,#l,#l,#l,#l,#r,#r,#r,#r,#r,
             #l,#l,#l,#l,#l,#r,#r,#r,#r,#r,#r,
              #l,#l,#l,#l,#l,#r,#r,#r,#r,#r
        ]
        let fingers = [
            #p,#r,#m,#m,#i,#i,#i,#m,#r,#p,
             #p,#r,#m,#i,#i,#i,#i,#m,#r,#p,#p,
              #p,#r,#i,#i,#i,#i,#i,#m,#r,#p
        ]
        let cols = [
            0,1,2,3,4,5,6,7,8,9,
             0,1,2,3,4,5,6,7,8,9,10,
              0,1,2,3,4,5,6,7,8,9
        ]
        let rows = [
            0,0,0,0,0,0,0,0,0,0,
             1,1,1,1,1,1,1,1,1,1,1,
              2,2,2,2,2,2,2,2,2,2
        ]
        in {codes, fingers, cols, rows, hands, num_fingers = 4}

    module Strokes = { 
        let all = layout_to_strokes default
        
        -- which is faster?
        let same_finger_v2 hand finger =
            let bits = map (\stroke -> if stroke.hand == hand && stroke.finger == finger then 1 else 0) all
            let scanned = scan (+) 0 bits
            let selected = map2 (*) scanned bits
            let indices = map (\x -> x - 1) selected
            let count = reduce (+) 0 bits
            in spread count Stroke.empty indices all

        let same_finger_raw hand finger = filter (\(_,stroke) -> stroke.finger == finger && stroke.hand == hand) (zip (iota 31) all)
        let same_finger_indices hand finger = same_finger_raw hand finger |> map (.0)
        let same_finger_v1 hand finger = same_finger_raw hand finger |> map (.1)
        let same_finger = same_finger_v1

        let same_hand_raw hand = filter (\(_,stroke) -> stroke.hand == hand) (zip (iota 31) all)
        let same_hand_indices hand = same_hand_raw hand |> map (.0)
        let same_hand hand = same_hand_raw hand |> map (.1)

        let same_row_raw hand row = filter (\(_,stroke) -> stroke.hand == hand && stroke.row == row) (zip (iota 31) all)
        let same_row_indices hand row = same_row_raw hand row |> map (.0)
        let same_row hand row = same_row_raw hand row |> map (.1)

    }
}

module Stats = {
    let sfbs [n] [m] (corpus_item: Corpus.Item.t [n]) (layout: Layout.t[m])  =
        map (\(hand, finger) ->
            let strokes = Ansi31DAM.Strokes.same_finger hand finger 
            let keys = map (\stroke -> (stroke, layout.codes[stroke.index])) strokes
            let pairs = cartesian_product keys keys
            let x =
                loop x = 0
                for i < (length pairs) do 
                let k1 = pairs[i].0.1
                let k2 = pairs[i].1.1
                in x f32.+ corpus_item.raw[k1][k2]
            in x
        ) Hf.all
}