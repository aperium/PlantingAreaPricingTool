# Planting Area Pricing Tool

Live Demo: [https://greenstreet.shinyapps.io/BedPlanningTool/](https://greenstreet.shinyapps.io/BedPlanningTool/)

All information is provided without any promise or accuracy stated or implied.

## Planned Improvements

- [x] accept more flexible freestyle input units including `feet` & `'`.
- [x] require account number to reveal customer-specific pricing.
- [ ] Allow users to use custom planting densities for each product type
- [ ] Help users with beds of different shapes.
  - [x] either a help box with formulas for the areas of different shapes or a selector for users to choose the shape of bed they have.
  - [ ] Would still be nice to have a tool help. See `shiny::radioButtons()`
  - [ ] Shapes of beds to include:
    - [ ] rectangle
    - [ ] triangle
    - [ ] circle
    - [ ] (maybe also oval and half circle?)
- [x] Add `plant spacing` as a column/option alongside `plant density`.
- [ ] Add user customer number to be passed in the URL. See `shiny::getQueryString()`
- [ ] Update account number entry to use `shiny::passwordInput()`?
