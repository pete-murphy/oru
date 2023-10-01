/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./build/main.js"],
  theme: {
    extend: {},
  },
  plugins: [
    require("tailwindcss/plugin")(({ addBase }) => {
      addBase({
        '[type="search"]::-webkit-search-decoration': { display: "none" },
        '[type="search"]::-webkit-search-cancel-button': { display: "none" },
        '[type="search"]::-webkit-search-results-button': { display: "none" },
        '[type="search"]::-webkit-search-results-decoration': {
          display: "none",
        },
      });
    }),
  ],
};
