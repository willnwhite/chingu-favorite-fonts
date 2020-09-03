# Chingu

This is one of [Chingu](https://chingu.io)'s Explorer projects, Favorite Fonts. It's essentially a clone of the Google Fonts frontend. The [project specification](https://github.com/chingu-voyages/soloproject-tier2-favfonts/blob/c696272a6c041ba3c457b85ce0979b92295df293/README.md) has more details.

# Live demo

The app is running at http://favorite-fonts-willw.surge.sh. If you want to run it locally:

1. Compile `Main.elm` to `main.js` using the Elm compiler ([get it here](https://guide.elm-lang.org/install/elm.html)). `main.js` must end up in the same directory as `index.html`. You can achieve that with this command: `elm make Main.elm --output=main.js [--optimize]`
2. Open index.html in your browser.

# App walkthrough

The value of this app is to show the user fonts, and these are requested from Google Fonts. Because the user can't see all (999) fonts at once, requesting them all at once would unnecessarily increase the page load size (and the burden on Google Fonts). The page therefore loads with the first eight or sixteen fonts, depending on how many are necessary to fill the page, and further requests are made when the user scrolls to the bottom of the page. Eight fonts are loaded at a time, and this number can be configured by changing `fontsPerRequest` in `Main.elm`. After a certain amount of scrolling down, a "Back to top" button appears in the bottom-right corner.

In addition to this view of all the fonts, the user can search for fonts using the "Search" input. It's possible that some of the fonts found in the search have already been requested in the view of all the fonts, so to avoid requesting those again the app keeps track of which fonts have already been requested, and only those that haven't will be included in the next request. Conversely, fonts that have been requested by search will not be re-requested when scrolling down through the view of all the fonts.

A couple of extra points:

- Though there are links to other parts of the website in the header, these have no effect.
- The "Add" button on each font has no effect.

# Code structure

Types are moved out into their respective modules: Font, Fonts, LoadedAndUnloadedFonts and RequestedFonts. This means that the interface between Main and the type can be controlled, so that Main doesn't depend on the internal implementation of the type. Also, view code moved out into Header and MajorNavigation reduces the amount of lower-level code in Main.

# Testing

Only a couple of tests are enumerated here to show how the code _would_ be tested. To run these tests, run [`elm-test`](https://package.elm-lang.org/packages/elm-explorations/test/latest/).

# Development timeline

I developed the project within four working weeks. As the Git history shows, within the first week the user had something they could use to view and search fonts. The rest of the time was spent on layout and responsiveness, styling, loading fonts by scrolling down, optimising requests, and code refactoring.

# Further work

- Make search results load n-at-a-time too.
- Guarantee that enough fonts will be loaded on page load to fill the screen (i.e. more than sixteen if the window is tall enough).
- Use CSS for layout and styling, perhaps using elm-css or another CSS pre-processor.
- Refactor the code with the approach of "making impossible states unrepresentable": https://www.youtube.com/watch?v=IcgmSRJHu_8
- Write automated tests for the project.
- Investigate whether the CSS Font Loading API could replace stylesheet links for requesting fonts. The problem with stylesheet links is that it has to be assumed that requests are always successful.
