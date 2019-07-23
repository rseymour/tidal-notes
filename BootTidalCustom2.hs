:set -XOverloadedStrings
:set prompt ""
:set prompt-cont ""

import Sound.Tidal.Context

-- total latency = oLatency + cFrameTimespan
tidal <- startTidal (superdirtTarget {oLatency = 0.1, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cFrameTimespan = 1/20})

let p = streamReplace tidal
let hush = streamHush tidal
let list = streamList tidal
let mute = streamMute tidal
let unmute = streamUnmute tidal
let solo = streamSolo tidal
let unsolo = streamUnsolo tidal
let once = streamOnce tidal
let asap = once
let nudgeAll = streamNudgeAll tidal
let all = streamAll tidal
let resetCycles = streamResetCycles tidal
let setcps = asap . cps
let xfade i = transition tidal True (Sound.Tidal.Transition.xfadeIn 4) i
let xfadeIn i t = transition tidal True (Sound.Tidal.Transition.xfadeIn t) i
let histpan i t = transition tidal True (Sound.Tidal.Transition.histpan t) i
let wait i t = transition tidal True (Sound.Tidal.Transition.wait t) i
let waitT i f t = transition tidal True (Sound.Tidal.Transition.waitT f t) i
let jump i = transition tidal True (Sound.Tidal.Transition.jump) i
let jumpIn i t = transition tidal True (Sound.Tidal.Transition.jumpIn t) i
let jumpIn' i t = transition tidal True (Sound.Tidal.Transition.jumpIn' t) i
let jumpMod i t = transition tidal True (Sound.Tidal.Transition.jumpMod t) i
let mortal i lifespan release = transition tidal True (Sound.Tidal.Transition.mortal lifespan release) i
let interpolate i = transition tidal True (Sound.Tidal.Transition.interpolate) i
let interpolateIn i t = transition tidal True (Sound.Tidal.Transition.interpolateIn t) i
let clutch i = transition tidal True (Sound.Tidal.Transition.clutch) i
let clutchIn i t = transition tidal True (Sound.Tidal.Transition.clutchIn t) i
let anticipate i = transition tidal True (Sound.Tidal.Transition.anticipate) i
let anticipateIn i t = transition tidal True (Sound.Tidal.Transition.anticipateIn t) i
let forId i t = transition tidal False (Sound.Tidal.Transition.mortalOverlay t) i
let d1 = p 1
let d2 = p 2
let d3 = p 3
let d4 = p 4
let d5 = p 5
let d6 = p 6
let d7 = p 7
let d8 = p 8
let d9 = p 9
let d10 = p 10
let d11 = p 11
let d12 = p 12
let d13 = p 13
let d14 = p 14
let d15 = p 15
let d16 = p 16

:set prompt "tidal> "

import qualified Sound.Tidal.Scales as Scales

import qualified Sound.Tidal.Chords as Chords

import Sound.Tidal.Utils

import Sound.Tidal.Params

import Data.Maybe

import Control.Applicative

let bpm b = setcps (b/2/120)

-- FX groups
let adsr = grp [mF "attack",  mF "decay", mF "sustain", mF "release"]
let del  = grp [mF "delay",   mF "delaytime", mF "delayfeedback"]
let scc  = grp [mF "shape",   mF "coarse", mF "crush"]
let spa  = grp [mF "speed",   mF "accelerate"]
let rvb  = grp [mF "room",    mF "size"]
let gco  = grp [mF "gain",    mF "cut", mF "orbit"]
let glo  = grp [mF "gain",    mF "legato", mF "orbit"]
let go   = grp [mF "gain",    mF "orbit"]
let io   = grp [mF "begin",   mF "end"]
let eq   = grp [mF "cutoff",  mF "resonance", mF "bandf", mF "bandq", mF "hcutoff", mF "hresonance"]
let tremolo = grp [mF "tremolorate", mF "tremolodepth"]
let phaser  = grp [mF "phaserrate", mF "phaserdepth"]
-- TODO: add SpectralTricks / SC FX groups
-- FX groups' function version
let adsr' a d s r = attack a # decay d # sustain s # release r
let del' l t f = delay l # delaytime t # delayfeedback f
let scc' s c c' = shape s # coarse c # crush c'
let lpf' c r = cutoff c # resonance r
let bpf' f q = bandf f # bandq q
let hpf' c r = hcutoff c # hresonance r
let spa' s a = speed s # accelerate a
let gco' g c o = gain g # cut c # orbit o
let glo' g l o = gain g # legato l # orbit o
let go' g o = gain g # orbit o
let rvb' r s = room r # size s
let io' i o  = begin i # end o
let eq h b l q = cutoff l # resonance q # bandf b # bandq q # hcutoff h # hresonance q
let tremolo' r d = tremolorate r # tremolodepth d
let phaser' r d = phaserrate r # phaserdepth d

let r = run
let ri a = rev (r a) -- run inverted
let rodd a = (((r a) + 1) * 2) - 1 -- run of odd numbers
let reven a = ((r a) + 1) * 2 -- run of even numbers
let roddi a = rev (rodd a) -- run of odd numbers inverted
let reveni a = rev (reven a) -- run of even numbers inverted

let c = choose
let codd a = c $ take a [1,3..] -- choose an odd number
let ceven a = c $ take a [0,2..] -- choose an even number

-- TODO: primes ..?

-- transitions
let j p n  = jumpIn' p n
let j2 p   = jumpIn' p 2
let j4 p   = jumpIn' p 4
let j8 p   = jumpIn' p 8
let j16 p  = jumpIn' p 16
let xf p n = xfadeIn  p n
let xf2 p  = xfadeIn  p 2
let xf4 p  = xfadeIn  p 4
let xf8 p  = xfadeIn  p 8
let xf16 p = xfadeIn  p 16
let cl p n = clutchIn p n
let cl2 p  = clutchIn p 2
let cl4 p  = clutchIn p 4
let cl8 p  = clutchIn p 8
let cl16 p = clutchIn p 16

-- continous function shorthands
let sin = sine
let cos = cosine
let sq  = square
let pulse w = sig $ \t -> if ((snd $ properFraction t) >= w) then 1.0 else 0.0
let pulse' w = liftA2 (\a b -> if (a>=b) then 1.0 else 0.0) saw w
let pw = pulse
let pw' = pulse'

-- range shorthands
let range' from to p = (p*to - p*from) + from
let rg' = range'
let rg = range -- old: scale
let rgx = rangex -- old: scalex

-- continuous at freq
let sinf  f = fast f $ sin -- sine at freq
let cosf  f = fast f $ cos -- cosine at freq
let trif  f = fast f $ tri -- triangle at freq
let sawf  f = fast f $ saw -- saw at freq
let isawf f = fast f $ isaw -- inverted saw at freq
let sqf   f = fast f $ sq -- square at freq
let pwf w f = fast f $ pw w -- pulse at freq
let pwf' w f = fast f $ pw' w -- pulse' at freq
let randf f = fast f $ rand -- rand at freq

-- ranged continuous
let rsin i o = rg' i o sin -- ranged' sine
let rcos i o = rg' i o cos -- ranged' cosine
let rtri i o = rg' i o tri -- ranged' triangle
let rsaw i o = rg' i o saw -- ranged' saw
let risaw i o = rg' i o isaw -- ranged' inverted saw
let rsq i o = rg' i o sq -- ranged' square
-- rpw i o w = rg' i o pw w -- ranged' pulse
-- rpw' i o w = rg' i o pw' w -- ranged' pulse'
let rrand i o = rg' i o rand -- ranged' rand
let rxsin i o = rgx i o sin -- ranged' exponential sine
let rxcos i o = rgx i o cos -- ranged' exponential cosine
let rxtri i o = rgx i o tri -- ranged' exponential triangle
let rxsaw i o = rgx i o saw -- ranged' exponential saw
let rxisaw i o = rgx i o isaw -- ranged' exponential inverted saw
let rxsq  i o = rgx i o sq -- ranged' exponential sqaure
let rxpw i o w = rgx i o pw w -- ranged' exponential pulse
let rxpw' i o w = rgx i o pw' w -- ranged' exponential pulse'
let rxrand i o = rgx i o rand -- ranged' exponential rand

-- ranged continuous at freq
let rsinf i o f = fast f $ rsin i o -- ranged' sine at freq
let rcosf i o f = fast f $ rcos i o -- ranged' cosine at freq
let rtrif i o f = fast f $ rtri i o -- ranged' triangle at freq
let rsawf i o f = fast f $ rsaw i o -- ranged' saw at freq
let risawf i o f = fast f $ risaw i o  -- ranged' inverted saw at freq
let rsqf i o f = fast f $ rsq i o  -- ranged' square at freq
-- rpwf i o w f = fast f $ rpw' i o w -- ranged' pulse at freq
let rrandf i o f = fast f $ rrand i o -- ranged' rand at freq
let rxsinf i o f = fast f $ rxsin i o -- ranged' exponential sine at freq
let rxcosf i o f = fast f $ rxcos i o -- ranged' exponential cosine at freq
let rxtrif i o f = fast f $ rxtri i o -- ranged' exponential triangle at freq
let rxsawf i o f = fast f $ rxsaw i o -- ranged' exponential saw at freq
let rxisawf i o f = fast f $ rxisaw i o -- ranged' exponential inverted saw at freq
let rxsqf i o f = fast f $ rxsq i o -- ranged' exponential square at freq
let rxpwf i o w f = fast f $ rxpw i o w -- ranged' exponential pulse at freq
let rxpwf' i o w f = fast f $ rxpw' i o w -- ranged' exponential pulse' at freq
let rxrandf i o f = fast f $ rxrand i o  -- ranged' exponential random at freq
-- random shit
let screw l c p = loopAt l $ chop c $ p
-- mute p = (const $ sound "~") p
let toggle t f p = if (1 == t) then f $ p else id $ p
let tog = toggle

-- sound bank protoype https://github.com/tidalcycles/Tidal/issues/231
-- bank p = with s_p (liftA2 (++) (p::Pattern String))
-- b = bank

-- shorthands
let str = striate
let str' = striate'
let fE = foldEvery
let ev = every
let oa = offadd
let sp = speed
let ac = accelerate
let sl = slow
let fa = fast
let m = mute
let i = id
let g = gain
let o = orbit
let u = up
-- (>) = (#)
let deg = degrade
let degBy = degradeBy
let seg = segment

-- limit values in a Pattern to n equally spaced divisions of 1.
-- quantise' :: (Functor f, RealFrac b) => b -> f b -> f b
let quantise' n = fmap ((/n) . (fromIntegral :: RealFrac b => Int -> b) . round . (*n))

-- convert continuous functions to floats, ints, melodies x / x' (struct version)
let c2f  t p = seg t $ p -- continuous to floats
let c2f' t p = struct t $ p -- continuous to structured floats
let c2i  t p = quantise' 1 $ c2f t p -- continuous to ints
let c2i' t p = quantise' 1 $ c2f' t p -- continuous to structured ints
let c2m  s t p = scale s $ round <$> (c2f t p) -- continuous to melodic scale
let c2m' s t p = scale s $ round <$> (c2f' t p) -- continuous to structured melodic scale

-- harmony
let chordTable = Chords.chordTable
let scaleList = Scales.scaleList
let majork = ["major", "minor", "minor", "major", "major", "minor", "dim7"]
let minork = ["minor", "minor", "major", "minor", "major", "major", "major"]
let doriank = ["minor", "minor", "major", "major", "minor", "dim7", "major"]
let phrygiank = ["minor", "major", "major", "minor", "dim7", "major", "minor"]
let lydiank = ["major", "major", "minor", "dim7", "major", "minor", "minor"]
let mixolydiank = ["major", "minor", "dim7", "major", "minor", "minor", "major"]
let locriank = ["dim7", "major", "minor", "minor", "major", "major", "minor"]
let keyTable = [("major", majork),("minor", minork),("dorian", doriank),("phrygian", phrygiank),("lydian", lydiank),("mixolydian", mixolydiank),("locrian", locriank),("ionian", majork),("aeolian", minork)]
let keyL p = (\name -> fromMaybe [] $ lookup name keyTable) <$> p
-- | @chord p@ turns a pattern of chord names into a pattern of
-- numbers, representing note value offsets for the chords
-- chord :: Num a => Pattern String -> Pattern a
let chord p = flatpat $ Chords.chordL p
let harmonise ch p = scale ch p + chord (flip (!!!) <$> p <*> keyL ch)

-- midi
-- cc = grp [mF "ccn", mF "ccv"]
-- ccn :: Pattern Double -> ControlPattern
-- ccn = pF "ccn"
-- ccv :: Pattern Double -> ControlPattern
-- ccv = pF "ccv"
-- m val = range 0 127 $ val
let toM val = rg 0 127 val
let cc' p n = ccv (toM p) # ccn n

let oldm p = (const $ sound "~") p

-- mute/solo
let mutePatterns g = mapM (streamMute tidal) g
let muteIntPatterns g = mutePatterns (map show g)
let mutePatterns' s g = mutePatterns (fromJust $ lookup g s)
let unmutePatterns g = mapM (streamUnmute tidal) g
let unmuteIntPatterns g = unmutePatterns (map show g)
let unmutePatterns' s g = unmutePatterns (fromJust $ lookup g s)
let soloPatterns g = mapM (streamSolo tidal) g
let soloPatterns' s g = soloPatterns (fromJust $ lookup g s)
let unsoloPatterns g = mapM (streamUnsolo tidal) g
let unsoloPatterns' s g = unsoloPatterns (fromJust $ lookup g s)
let muteTrackPatterns t g = mapM (streamMute tidal) (map ((t ++ "-") ++) g)
let muteTrackIntPatterns t g = muteTrackPatterns t (map show g)
let muteTrackPatterns' t s g = muteTrackPatterns (fromJust $ lookup (map ((t ++ "-") ++) g) s)
let unmuteTrackPatterns t g = mapM (streamUnmute tidal) (map ((t ++ "-") ++) g)
let unmuteTrackIntPatterns t g = unmuteTrackPatterns t (map show g)
let unmuteTrackPatterns' t s g = unmuteTrackPatterns (fromJust $ lookup (map ((t ++ "-") ++) g) s)
let soloTrackPatterns t g = mapM (streamSolo tidal) (map ((t ++ "-") ++) g)
let soloTrackPatterns' t s g = soloTrackPatterns (fromJust $ lookup (map ((t ++ "-") ++) g) s)
let unsoloTrackPatterns t g = mapM (streamUnsolo tidal) (map ((t ++ "-") ++) g)
let unsoloTrackPatterns' t s g = unsoloTrackPatterns (fromJust $ lookup (map ((t ++ "-") ++) g) s)
let mp  = mutePatterns
let md  = muteIntPatterns
let mp' = mutePatterns'
let ump = unmutePatterns
let umd = unmuteIntPatterns
let ump' = unmutePatterns'
-- sp = soloPatterns
-- sp' = soloPatterns'
let usp = unsoloPatterns
let usp' = unsoloPatterns'
let mtp = muteTrackPatterns
let mtd = muteTrackIntPatterns
let mtp' = muteTrackPatterns'
let umtp = unmuteTrackPatterns
let umtd = unmuteTrackIntPatterns
let umtp' = unmuteTrackPatterns'
let stp = soloTrackPatterns
let stp' = soloTrackPatterns'
let ustp = unsoloTrackPatterns
let ustp' = unsoloTrackPatterns'

-- apply function from map
let f fs n = fromJust $ lookup n fs
