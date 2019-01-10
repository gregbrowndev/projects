const gulp = require('gulp');
const browserSync = require('browser-sync').create();
const sass = require('gulp-sass');
const autoprefixer = require('gulp-autoprefixer');

// Compile Sass & Inject Into Browser
function compile() {
  return gulp.src([
    'node_modules/bootstrap/scss/bootstrap.scss',
    'src/scss/*.scss'
  ])
    .pipe(sass({ includePaths: ['node_modules'] }))
    .pipe(autoprefixer({
      browsers: ['last 2 versions'],
      cascade: false
    }))
    .pipe(gulp.dest('dist/css'))
    .pipe(browserSync.stream());
}

function js() {
  return gulp.src([
    'node_modules/bootstrap/dist/js/bootstrap.min.js',
    'node_modules/jquery/dist/jquery.min.js',
    'node_modules/popper.js/dist/umd/popper.min.js',
    'src/js/*.js'
  ])
    .pipe(gulp.dest('dist/js'))
    .pipe(browserSync.stream());
}

function html() {
  return gulp.src([
    'src/*.html'
  ])
    .pipe(gulp.dest('dist/'))
    .pipe(browserSync.stream());
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


// Watch Sass & Serve
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
  gulp.watch(['src/js/*.js'], gulp.series(js));
  gulp.watch('src/*.html', { events: 'change' }, gulp.series(html, reload));
}

const dev = gulp.series(compile, js, html, fonts, serve, watch);

// Default Task
exports.default = dev;