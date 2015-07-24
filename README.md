# dom-gnats: FRP and UI

## Ideas

### Coordinating input and output
Combinators use phantom type variables a la ST/STRef to tie VDOM event inputs to their elements.
This allows the routing combinators to be sure that inputs and outputs go together when constructing routes.

### Spatial and temporal composition
Spatial composition composes elements next to or underneath other elements, thus determining (along with styling attributes of course) their position and appearance. Causal composition lets elements alter the properties of other elements when interacted with, or even replace elements. It is the method of "wiring" the interactions of elements together to make things happen.

### Quasiquoting
Compose HTML into signal functions. Label event inputs to be available as names to anti-quoted signal functions.
