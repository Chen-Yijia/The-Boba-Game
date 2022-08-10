# The-Boba-Game

### Run Application Guide

1. Open the `app.R` file and click "Run App" to launch the game;
2. You are recommended to open the game in browser and use the full screen to get the best experience when playing the game;
3. Don't panic if the images are not loaded when you start the game for the first time! This is because some of our images are hosted on the external database and the file sizes for the graphics are quite big. Please wait patiently for the images to be rendered correctly!
4. Enjoy playing!

### File Navigation Guide

1. `components` folder has the front-end implementations
    - `loginPage`
        - `loginPageUI.R` has the register and login components.
    - `mainPage`
        - `mapUI.R` renders the home page background GIFs.
        - `mainPageUI.R` has the skeletal structue of the main game page and the UI for the score bar.
        - `notification.R` has the UI implementation of the events.
        - `actionPanel.R` renders the sliders, menus, and demand-price graph.
        - `displayPanel.R` renders the confirmation panel showing the current ingredient quantity and beverage prices.
    - `popupPage`
        - `popupPageUI.R` has the content for the deficiency popup and beverage menu popup in `popupPageUI.R`.
    - `helpPage`
        - `helpPageUI.R` contains the game manual and instructions.
    - `morePage`
        - `morePageUI.R` implements quit game and restart game functionalities.
    - `leaderboardPage`
        - `leaderPageUI.R` shows the overall leaderboard when the game ends.

2. `serverFunctions` folder has the important server functions
    - `mainServer.R` has the server codes to deal with button clicks and events, as well as render text, images and plots.
    - `mainFunctions.R` has the three main mathematical calculation functions for deciding if there is a deficiency, calculate the game metrics, and change the demand parameters in the event of contingencies.

3. `www` folder has the css styling script for the user interface design, all the images and gifs used in the game, and background music used.
    - `bootsrap.css` has all the self-defined component-specific stylings.

4. Historical demand-day and price-demand data
    - `Historical Sale copy.csv`
    - `PriceDemand.csv`

### Credits

Canva: animated GIFs, pictures for the user interface design.
<https://www.canva.com/en_gb/login/?shouldClearGotAutoSelect>

Imagebb: upload local images on the online database.
<https://imgbb.com>

Flaticon: graphics for ingredients and beverages.
<https://www.flaticon.com>

Background music.
<https://www.youtube.com/watch?v=ieL3VO4VWxE>
