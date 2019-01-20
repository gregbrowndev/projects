const gulp = require('gulp');
const sourcemaps = require('gulp-sourcemaps');
const sass = require('gulp-sass');
const autoprefixer = require('gulp-autoprefixer');
const imagemin = require('gulp-imagemin');
const browserify = require('browserify');
const watchify = require('watchify');
const babel = require('babelify');
const browserSync = require('browser-sync').create();
const source = require('vinyl-source-stream');
const buffer = require('vinyl-buffer');
const log = require('gulplog');


// Compile SASS

function compile() {
  return gulp.src([
    'node_modules/slick-carousel/slick/*.scss',
    'src/scss/*.scss',
  ])
    .pipe(sass({ includePaths: ['node_modules'] }))
    .pipe(autoprefixer({
      browsers: ['last 2 versions'],
      cascade: false
    }))
    .pipe(gulp.dest('dist/css'))
    .pipe(browserSync.stream());
}

// JavaScript

const customOpts = {
  entries: ['./src/js/main.js'],
  // require: ['bootstrap', 'jquery'],  // Note this breaks my exported functions
  standalone: 'mainBundle',
  debug: true
};
const opts = { ...watchify.args, ...customOpts };
const b = watchify(browserify(opts));
console.log('Browserify options: ', opts);

// add transformations here
// i.e. b.transform(coffeeify);
b.transform(babel, {
  presets: ['@babel/preset-env']
});

exports.js = bundle; // so you can run `gulp js` to build the file
b.on('update', bundle); // on any dep update, runs the bundler
b.on('log', log.info); // output build logs to terminal

function bundle() {
  return b.bundle()
  // log errors if they happen
    .on('error', log.error.bind(log, 'Browserify Error'))
    .pipe(source('bundle.js'))
    // optional, remove if you don't need to buffer file contents
    .pipe(buffer())
    // optional, remove if you dont want sourcemaps
    .pipe(sourcemaps.init({ loadMaps: true })) // loads map from browserify file
    // Add transformation tasks to the pipeline here.
    .pipe(sourcemaps.write('./')) // writes .map file
    .pipe(gulp.dest('./dist/js'))
    .pipe(browserSync.stream());
}

// Other assets

function html() {
  return gulp.src([
    'src/*.html'
  ])
    .pipe(gulp.dest('dist/'))
    .pipe(browserSync.stream());
}

function otherCSS() {
  return gulp.src([
    'node_modules/ekko-lightbox/dist/ekko-lightbox.css'
  ])
    .pipe(gulp.dest('dist/css'))
    .pipe(browserSync.stream());
}

function images() {
  return gulp.src([
    'src/img/**'
  ])
    .pipe(imagemin())
    .pipe(gulp.dest('dist/img/'));
}

// Move Fonts to dist/fonts
function fonts() {
  return gulp.src('node_modules/@fortawesome/fontawesome-free/webfonts/*')
    .pipe(gulp.dest('dist/webfonts'))
}

// Move Font Awesome CSS to dist/css
// function fa() {
//   return gulp.src('node_modules/@fortawesome/fontawesome-free/css/fontawesome.css')
//     .pipe(gulp.dest('dist/css'))
// }

// BrowserSync

function reload(done) {
  browserSync.reload();
  done();
}

function serve(done) {
  browserSync.init({
    server: './dist'
  });
  done();
}

function watch() {
  gulp.watch(['src/scss/*.scss'], gulp.series(compile));
  gulp.watch(['src/img/**'], gulp.series(images));
  gulp.watch('src/*.html', { events: 'change' }, gulp.series(html, reload));
}

const dev = gulp.series(gulp.parallel(compile, otherCSS, bundle, html, images, fonts), serve, watch);

// Default Task
exports.default = dev;