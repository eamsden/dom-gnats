# dom-gnats: FRP and UI

## Ideas

### Coordinating input and output
Use a newtype wrapper and force transforming signal functions to be parametric in their input and output left side.

    newtype Widget (widgetTree :: Tree) (svIn :: SV *) (svOut :: SV *)

    withWidget :: (forall widgetIn widgetOut . (widgetIn :^: svIn :~> widgetOut :^: svOut) -> (widgetIn :^: svIn' :~> widgetOut :^: svOut'))
               -> Widget widgetTree svIn svOut -> Widget widgetTree svIn' svOut'

### Spatial and temporal composition
Spatial composition composes elements next to or underneath other elements, thus determining (along with styling attributes of course) their position and appearance. Causal composition lets elements alter the properties of other elements when interacted with, or even replace elements. It is the method of "wiring" the interactions of elements together to make things happen.

### Quasiquoting
Compose HTML into signal functions. Label event inputs to be available as names to anti-quoted signal functions.
