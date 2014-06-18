Color Convert
========

A program to convert colors between different formats (hexadecimal, rgb, hsl).
Inspired by http://rem.im/rgb2hex.html, written in [elm] (http://elm-lang.org)

[Run] (http://notnew.github.io/color-convert/)
-------

Usage
-----

Convert between hexadecimal, rgb and hsl formats by filling out the appropriate field.  The rgb and hsl fields are displayed like the function calls in elm required to make the input color.

Fields are lenient in the input they accept, all these are accepted as the same color:

  - `150 180 255`
  - `  150, 180 255`
  - `rgb(150,180,255)`
  - `rgb(  150,180 , 255)`
  - `150 180 3000`

Hitting the enter key will reprint all the fields in a standard format (useful for displaying elm function calls that can be copied and pasted into code).

The hsl field takes a hue given in radians, and saturation and lightness given as a fraction between 0 and 1.

