const gulp = require('gulp');
const browserSync = require('browser-sync').create();
const sass = require('gulp-sass');
const autoprefixer = require('gulp-autoprefixer');

// Compile Sass & Inject Into Browser
function compile() {
  return gulp.src(['src/scss/*.scss'])
    .pipe(sass())
    .pipe(autoprefixer({
      browsers: ['last 2 versions'],
      cascade: false
    }))
    .pipe(gulp.dest('src/css'))
    .pipe(browserSync.stream());
}

// Watch Sass & Serve
function reload(done) {
  browserSync.reload();
  done();
}

function serve(done) {
  browserSync.init({
    server: './src'
  });
  done();
}

function watch() {
  gulp.watch(['src/scss/*.scss'], gulp.series(compile));
  gulp.watch('src/*.html', { events: 'change' }, gulp.series(reload));
}

const dev = gulp.series(compile, serve, watch);

// Default Task
exports.default = dev;