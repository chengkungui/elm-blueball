import Mouse
import Window
import Random
import Text

-- CONFIG

speed = 400
sizePill = 15
sizePlayer = sizePill

spawnInterval = 57 / speed

(width, height) = (400, 400)
(hWidth, hHeight) = (width / 2, height /2)

--HELPER FUNCTIONS

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

tf: Float -> Float -> String -> Form 
tf y scl str = toText str |> Text.color gray
                          |> centered 
                          |> toForm 
                          |> scale scl
                          |> move (0, y)

-- MODEL

type Pill = {pos : Vec, vel : Vec, rad : Float, col : Color}

defaultPill = {pos = (0, hHeight),
    vel = (0, -speed),
    rad = sizePill,
    col = lightRed}

newPill: Float -> Color -> Pill
newPill x col = {defaultPill | pos <- (x, hHeight), col <- col}

defaultPlayer = {defaultPill | pos <- (0, -hHeight - sizePlayer),
                               col <- black,
                               rad <- sizePlayer}

data State = Play | Over | Start 

type Game = {player: Pill, pills: [Pill], score: Int, state: State}

defaultGame = {player = defaultPlayer, pills = [], score = 0, state = Start}

-- UPDATE

data Event = Tick (Time, (Int, Int)) | Add Pill | Click

stepPlay : Event -> Game -> Game
stepPlay event g = 
    case event of
        Tick (t, mp) -> let hit pill = (vecLen <| vecSub g.player.pos pill.pos) < (g.player.rad + pill.rad)
                            unculled = filter (\{pos, rad}-> snd pos + rad > -hHeight ) g.pills
                            untouched = filter (not << hit) unculled
                            touched = filter hit unculled
                            hitColor c = not <| isEmpty <| filter (\{col} -> col == c) touched
                            hitBlue = hitColor lightBlue
                            hitRed = hitColor lightRed
                            out = let (x, y) = mp in abs (toFloat x) > hWidth || abs(toFloat y) > hHeight
                            g' = {g | player <- stepPlayer mp g.player, 
                                      pills <- map (stepPill t) untouched, 
                                      score <- if hitBlue then g.score + 1 else g.score}
                        in  if hitRed  || out then {defaultGame | state <- Over, score <- g.score} else g'
        Add p        -> {g | pills <- p :: g.pills}
        Click        -> g

click event = case event of 
                Click -> True
                _     -> False    

stepGame : Event -> Game -> Game
stepGame event g = 
    let playGame = {defaultGame | state <- Play}
        toPlay = if click event then playGame else g
    in case g.state of
        Play -> stepPlay event g
        Start -> toPlay
        Over -> toPlay
    

stepPlayer : (Int, Int) -> Pill -> Pill
stepPlayer (x, y) p = {p | pos <- (toFloat x, toFloat y)}

stepPill : Time -> Pill -> Pill
stepPill t p = {p | pos <- vecAdd p.pos <| vecMulS p.vel t }



-- DISPLAY
render : (Int, Int) -> Game -> Element
render (w, h) g = 
    let formPill {rad, col, pos} = circle rad |> filled col |> move pos
        txts = case g.state of
                Play -> [tf 0 4 (show g.score)]
                Over -> [tf 70 4 "Game Over",
                         tf 0 4 (show g.score),
                         tf -50 2 "Click to Restart"] 
                Start -> [tf 70 4 "BluePill",
                         tf 0 2 "Click to Start"] 
        forms = txts ++ (map formPill <| g.player :: g.pills)
    in color lightGray <| container w h middle
                    <| color white
                    <| collage width height forms

--INPUT 

dalta = fps 30
input = (,) <~ lift inSeconds dalta
            ~ sampleOn dalta (lift2 relativePosition (lift center Window.dimensions) Mouse.position)

rand fn sig = lift fn (Random.float sig)
randX = rand (\r -> width * r - hWidth)
randCol = rand (\r -> if r < 0.15 then lightBlue else defaultPill.col)

interval = every (second * spawnInterval)
event = merges [lift Tick input, 
                lift2 (\x col -> Add (newPill x col)) (randX interval) (randCol interval),
                lift (\_ -> Click) Mouse.isDown]        

main = render <~ Window.dimensions ~ foldp stepGame defaultGame event