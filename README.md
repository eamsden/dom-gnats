# dom-gnats: FRP and UI

## Ideas

### Coordinating input and output
We use a path type (an idea borrowed from Alan Jeffrey's FRP implementation in AgdaJS) to separate widgets, and to match up output DOM nodes with incoming events attributed to those nodes. Widget constructors constrain the path variable of their input events and output DOM element to be the same, and spatial combinators force these paths either left or right. The result is that no two widgets have the same path, and a widget's input events have the same path as its output elements. We can use this to route events automagically back to the DOM elements which triggered them (generally due to user interaction).

### Spatial and temporal composition
Spatial composition composes elements next to or underneath other elements, thus determining (along with styling attributes of course) their position and appearance. Causal composition lets elements alter the properties of other elements when interacted with, or even replace elements. It is the method of "wiring" the interactions of elements together to make things happen.

### Quasiquoting
Compose HTML into signal functions. Label event inputs to be available as names to anti-quoted signal functions.
