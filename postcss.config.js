module.exports = {
  plugins: [
    require("tailwindcss"),
    require("autoprefixer"),
    require("postcss-elm-tailwind")(),
    ...(process.env.NODE_ENV === "production" ? [require("cssnano")] : []),
  ],
};
