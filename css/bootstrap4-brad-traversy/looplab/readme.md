# Bootstrap 4 Boiler

## Overview

This is a boilerplate project for starting Bootstrap 4 projects.


## Creating the SASS workflow

Brad Traversy has a detailed [YouTube video](https://www.youtube.com/watch?v=rmXVmfx3rNo&t=772s)
on setting up the SASS & Browser-sync workflow. There is a guide
on browsersync [here](https://browsersync.io/docs/gulp).

Here is a summary of how to set it up:

1. Install Gulp CLI globally:

    ```shell
    npm i -g gulp-cli
    ```

1. Initialise _package.json_: `npm init`

1. Install Browsersync and Gulp
 
    ```shell
    npm install browser-sync gulp --save-dev
    ```

1. Install Gulp plugins for compiling SASS to CSS:

    ```shell
    npm install gulp-sass gulp-autoprefixer --save-dev
    ```

