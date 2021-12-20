# shiny
Shiny Scripts

From https://stackoverflow.com/questions/22252472/how-to-change-the-color-of-an-svg-element

For your Shiny App, you can compute de CSS filter for converting an SVG element from Black to any color.

Use as follow :

```
color = set_color(list(r=0,g=164,b=214))
s = set_solver(color)
solve(s)$filter
```
