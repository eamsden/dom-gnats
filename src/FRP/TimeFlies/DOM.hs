{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module FRP.TimeFlies.DOM where

import           Data.JSString (JSString)
import qualified Data.JSString as JSString (unpack)
import           Data.JSString.Text (textToJSString, textFromJSString)
import qualified Data.Text as T
import           FRP.TimeFlies
import           GHCJS.VDOM
import           GHCJS.VDOM.Attribute (Attribute)
import           GHCJS.VDOM.Element (custom, text)
import           GHCJS.VDOM.Event
import           Prelude hiding (filter)

-- | Lifted kind for position of a widget in our DOM representation.
data WidgetPath = WidgetStop | WidgetLeft WidgetPath | WidgetRight WidgetPath

-- | Routing by widget paths
data WidgetSum (path :: WidgetPath) a where
  WidgetSumStop :: a -> WidgetSum path a
  WidgetSumLeft :: WidgetSum (WidgetLeft path) a -> WidgetSum path a
  WidgetSumRight :: WidgetSum (WidgetRight path) a -> WidgetSum path a

class RouteWidget (path :: WidgetPath) where
  routeWidget :: WidgetSum path a -> WidgetSum WidgetStop a

instance RouteWidget WidgetStop where
  routeWidget = id 

instance (RouteWidget path) => RouteWidget (WidgetLeft path) where
  routeWidget = routeWidget . WidgetSumLeft 

instance (RouteWidget path) => RouteWidget (WidgetRight path) where
  routeWidget = routeWidget . WidgetSumRight

-- | Combining by widget paths
data WidgetProd (path :: WidgetPath) a where
  WidgetProdUnit :: a -> WidgetProd path a
  WidgetProdConcat :: WidgetProd (WidgetLeft path) a -> WidgetProd (WidgetRight path) a -> WidgetProd path a
  WidgetProdParentChild :: a -> WidgetProd (WidgetRight path) a -> WidgetProd path a

-- | Two signal functions combined so that the input/output of the left is the left input/output
-- (mutatis mutandis right)
(***) :: svInLeft :~> svOutLeft -> svInRight :~> svOutRight -> (svInLeft :^: svInRight) :~> (svOutLeft :^: svOutRight)
left *** right = first left >>> second right

-- | Run two signal functions on a common input
(&&&) :: svIn :~> svOutLeft -> svIn :~> svOutRight -> svIn :~> (svOutLeft :^: svOutRight)
left &&& right = copy >>> (left *** right)

-- | Concatenate two widgets so that one follows the other in the DOM
concatWidgets :: ((SVEvent (WidgetSum (WidgetLeft path) a) :^: SVEvent (WidgetSum (WidgetRight path) a)) :^: svIn) 
                  :~> ((SVSignal (WidgetProd (WidgetLeft path) b) :^: SVSignal (WidgetProd (WidgetRight path) b)) :^: svOut)
              -> (SVEvent (WidgetSum path a) :^: svIn) :~> (SVSignal (WidgetProd path b) :^: svOut)
concatWidgets sv =
  let inputTransformer = 
        first $ filter widgetSumLeft &&& filter widgetSumRight
      outputTransformer = first $ combineSignals $ uncurry WidgetProdConcat
  in inputTransformer >>> sv >>> outputTransformer
  where
    widgetSumLeft :: WidgetSum path a -> Maybe (WidgetSum (WidgetLeft path) a)
    widgetSumLeft (WidgetSumLeft sum) = Just sum
    widgetSumLeft _ = Nothing

    widgetSumRight :: WidgetSum path a -> Maybe (WidgetSum (WidgetRight path) a)
    widgetSumRight (WidgetSumRight sum) = Just sum
    widgetSumRight _ = Nothing

-- | Make the left widget the parent of the right widget in the DOM
containWidget :: (forall c . (Children c) => [Attribute] -> c -> VNode)
              -> (SVEvent (WidgetSum (WidgetRight path) a) :^: svIn) 
                 :~> (SVSignal (WidgetProd (WidgetRight path) GnatDOM) :^: svOut)
              -> (SVEvent (WidgetSum path a) :^: svIn) :~> (SVSignal (WidgetProd path GnatDOM) :^: (SVEvent a :^: svOut))
containWidget parentF sv =
  let inputTransformer = copy >>>
                         second (second ignore >>> cancelRight >>> filter widgetSumStop) >>>
                         first (first $ filter widgetSumRight)
      middle = sv *** (uncancelLeft >>> (first $ constant $ Element parentF))
  in inputTransformer >>> middle >>> (first swap >>> associate >>> second unassociate >>> swap >>> associate >>> first (combineSignals $ uncurry $ flip WidgetProdParentChild))
  where
    widgetSumRight :: WidgetSum path a -> Maybe (WidgetSum (WidgetRight path) a)
    widgetSumRight (WidgetSumRight sum) = Just sum
    widgetSumRight _ = Nothing

    widgetSumStop :: WidgetSum path a -> Maybe a
    widgetSumStop (WidgetSumStop x) = Just x
    widgetSumStop _ = Nothing

-- | Gnats representation of DOM
data GnatDOM
  = Element (forall c. (Children c) => [Attribute] -> c -> VNode)
  | Text T.Text

-- | Compile an output tree to a VNode
compileGnat :: (RouteWidget path)
            => (WidgetSum WidgetStop GnatEvent -> IO ())
            -> WidgetProd path GnatDOM
            -> [VNode]
compileGnat _ (WidgetProdUnit (Text t)) = [text $ textToJSString t]
compileGnat handler prod@(WidgetProdUnit (Element f)) = 
  let eventAttributes = map ($ handler . routeWidgetProd prod . WidgetSumStop) gnatEvents
  in [f eventAttributes ()]
compileGnat handler (WidgetProdConcat left right) = compileGnat handler left ++ compileGnat handler right
compileGnat handler prod@(WidgetProdParentChild (Element parentF) child) =
  let eventAttributes = map ($ handler . routeWidgetProd prod . WidgetSumStop) gnatEvents
  in [parentF eventAttributes $ compileGnat handler child]
compileGnat handler prod@(WidgetProdParentChild (Text t) _) = -- need to fix this :\
  [text $ textToJSString t]

-- | Events to watch on each element
gnatEvents :: [(GnatEvent -> IO ()) -> Attribute]
gnatEvents = 
  [ mkMouseEvent click MouseClick
  , mkMouseEvent dblclick MouseDoubleClick
  , mkMouseEvent mousedown MouseDown
  , mkMouseEvent mouseenter MouseEnter
  , mkMouseEvent mouseleave MouseLeave
  , mkMouseEvent mousemove MouseMove
  , mkMouseEvent mouseout MouseOut
  , mkMouseEvent mouseover MouseOver
  , mkMouseEvent mouseup MouseUp
  , mkKeyEvent keydown KeyDown
  , mkKeyEvent keypress KeyPress
  , mkKeyEvent keyup KeyUp
  , mkFocusEvent focus Focus
  , mkFocusEvent blur Blur
  , mkGenericEvent submit Submit
  , mkGenericEvent change Change
  ]

  where
    mkMouseEvent :: ((MouseEvent -> IO ()) -> Attribute)
                 -> (MouseLocation -> GnatEvent)
                 -> (GnatEvent -> IO ())
                 -> Attribute
    mkMouseEvent attributeMaker constructor handler =
      attributeMaker (\mouseEvt -> handler (constructor $ MouseLocation (clientX mouseEvt) (clientY mouseEvt) (button mouseEvt)))

    mkKeyEvent :: ((KeyboardEvent -> IO ()) -> Attribute)
               -> (Key -> GnatEvent)
               -> (GnatEvent -> IO ())
               -> Attribute
    mkKeyEvent attributeMaker constructor handler =
      attributeMaker (\keyEvt -> handler (constructor $ Key (head $ JSString.unpack $ key keyEvt)
                                                            (ctrlKey keyEvt)
                                                            (metaKey keyEvt)
                                                            (shiftKey keyEvt)))

    mkFocusEvent :: ((FocusEvent -> IO ()) -> Attribute)
                 -> GnatEvent
                 -> (GnatEvent -> IO ())
                 -> Attribute
    mkFocusEvent attributeMaker constructor handler = 
      attributeMaker $ const $ handler constructor

    mkGenericEvent :: ((Event -> IO ()) -> Attribute)
                   -> GnatEvent
                   -> (GnatEvent -> IO ())
                   -> Attribute
    mkGenericEvent attributeMaker constructor handler =
     attributeMaker $ const $ handler constructor
              
   

-- | lets us force the type variable in the path
routeWidgetProd :: (RouteWidget path) => WidgetProd path b -> WidgetSum path a -> WidgetSum WidgetStop a
routeWidgetProd = const routeWidget

-- | Gnats representation of DOM event
data GnatEvent
  = MouseClick MouseLocation
  | MouseDoubleClick MouseLocation
  | MouseDown MouseLocation
  | MouseEnter MouseLocation
  | MouseLeave MouseLocation
  | MouseMove MouseLocation
  | MouseOut MouseLocation
  | MouseOver MouseLocation
  | MouseUp MouseLocation
  | MouseWheel WheelDelta
  | KeyDown Key 
  | KeyPress Key
  | KeyUp Key
  | Focus
  | Blur
  | Submit
  | Change

data MouseLocation
  = MouseLocation
    { _mouseX :: Int
    , _mouseY :: Int
    , _button  :: Int
    }

data WheelDelta
  = WheelDelta
    { _wheelDeltaX :: Double
    , _wheelDeltaY :: Double
    , _wheelDeltaZ :: Double
    , _wheelDeltaMode :: Double
    }

data Key
  = Key
  { _keyWhich :: Char
  , _keyCtrl :: Bool
  , _keyMeta :: Bool
  , _keyShift :: Bool
  }

domText :: T.Text -> (SVEvent (WidgetSum path GnatEvent) :^: SVEmpty) :~> (SVSignal (WidgetProd path GnatDOM) :^: SVEmpty)
domText text = ignore >>> (constant $ WidgetProdUnit $ Text text) >>> uncancelRight

domElement :: T.Text -> [Attribute] -> (SVEvent (WidgetSum path GnatEvent) :^: SVEmpty) :~> (SVSignal (WidgetProd path GnatDOM) :^: SVEvent GnatEvent)
domElement tag attributes =
  swap >>> (second $ filter sumStop) >>> (first $ constant $ WidgetProdUnit element)
  where
    sumStop :: WidgetSum path a -> Maybe a
    sumStop (WidgetSumStop x) = Just x
    sumStop _ = Nothing
    
    element :: GnatDOM
    element = Element (\addedAttributes children -> custom (textToJSString tag) (attributes ++ addedAttributes) children)

