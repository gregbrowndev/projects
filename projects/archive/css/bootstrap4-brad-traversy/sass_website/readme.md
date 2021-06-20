# Sass Website

## Overview

This project uses a SASS + Gulp workflow boilerplate following
Brad Traversy's [YouTube](https://www.youtube.com/watch?v=rmXVmfx3rNo&t=772s). 

See the bs4boiler project in this repo so details on how the workflow
was created.

## Notes

Some interesting points when developing the site:

1. Emmet is super powerful. 

    Emmet provides many template snippets and functions for writing HTML
    fast. Here are some examples of cool things you can do, by typing the 
    following text then hitting TAB:
    
    * `!` - generates boilerplate HTML contents
    * `li>a[href="#"]{Home}` - concise expression which generates a `li` 
        and child `a` complete with `href` attribute and text content
        (the stuff within `{...}`).
    * `lorem10` - generates 10 words
    
1. SASS provides a utility, `lightness`, which can be used to programmatically 
    choose a light or dark colour text depending on the background colour.

    ```scss
    @function set-text-color($bg-color) {
      @if(lightness($bg-color) > 50) {
        @return #000;
      } @else {
        @return #fff;
      }
    }
    
    header#main-header {
      padding: 1em;
      background: $primary-color;
      color: set-text-color($primary-color)
    }
    ```
    
    Now the text colour in the header will contrast better with the
    background.
    
1. Using SASS `darken` and `lighten` functions to get a different 
    shade of our colour:
    
    ```scss
    .button-a {
      background: darken($primary-color, 10%);
    }
    
    .button-b {
      background: lighten($seconday-color, 10%);
    }
    ```