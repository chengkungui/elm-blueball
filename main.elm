import Mouse
import Window
import Random

(width, height) = (400, 400)
(hWidth, hHeight) = (width / 2, height /2)

relativePosition: (Int, Int) -> (Int, Int) -> (Int, Int)
relativePosition (ox, oy) (x, y) = (x - ox, -(y - oy))

center : (Int, Int) -> (Int, Int)
center (w, h) = (w // 2, h // 2)


type Vec = (Float, Float)

vecAdd : Vec -> Vec -> Vec
vecAdd v1 v2 = (fst v1 + fst v2, snd v1 + snd v2)

vecSub : Vec -> Vec -> Vec
vecSub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

vecLen : Vec -> Float
vecLen (x, y) = sqrt (x * x + y * y)

vecMulS : Vec -> Time -> Vec
vecMulS  v t = (fst v * t, snd v * t)

type Pill = {pos : Vec, vel : Vec, rad : Float, col : Color}

defaultPill = {pos = (0, hHeight),
    vel = (0, -100),
    rad = 15,
    col = lightRed}

newPill: Float -> Color -> Pill
newPill x col = {defaultPill | pos <- (x, hHeight), col <- col}

defaultPlayer = {defaultPill | pos <- (0, 0),
                               col <- black}

type Game = {player: Pill, pills: [Pill]}

defaultGame = {player = defaultPlayer, pills = []}

data Event = Tick (Time, (Int, Int)) | Add Pill

stepGame : Event -> Game -> Game
stepGame event ({player, pills} as g) = 
    case event of
        Tick (t, mp) -> let hit pill = (vecLen <| vecSub player.pos pill.pos) < (player.rad + pill.rad)
                            unculled = filter (\{pos, rad}-> snd pos + rad > -hHeight ) pills
                            untouched = filter (not << hit) unculled
                        in {g | player <- stepPlayer mp player, pills <- map (stepPill t) untouched}
        Add p        -> {g | pills <- p :: g.pills}
    

stepPlayer : (Int, Int) -> Pill -> Pill
stepPlayer (x, y) p = {p | pos <- (toFloat x, toFloat y)}

stepPill : Time -> Pill -> Pill
stepPill t p = {p | pos <- vecAdd p.pos <| vecMulS p.vel t }

render : (Int, Int) -> Game -> Element
render (w, h) game = 
    let formPill {rad, col, pos} = circle rad |> filled col |> move pos
        forms = formPill game.player :: map formPill game.pills 
    in color lightGray <| container w h middle
                    <| color white
                    <| collage width height forms

dalta = fps 30
input = (,) <~ lift inSeconds dalta
            ~ sampleOn dalta (lift2 relativePosition (lift center Window.dimensions) Mouse.position)

rand fn sig = lift fn (Random.float sig)
randX = rand (\r -> width * r - hWidth)
randCol = rand (\r -> if r < 0.15 then lightBlue else defaultPill.col)

interval = every (second * 0.5)
event = merges [lift Tick input, 
                lift2 (\x col -> Add (newPill x col)) (randX interval) (randCol interval)]        

main = render <~ Window.dimensions ~ foldp stepGame defaultGame event