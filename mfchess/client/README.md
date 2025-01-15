# mfChess Web Frontend

## Dependencies
1. `NodeJS v18.13.0`
2. [`Better Comments VSCode Extension`](https://marketplace.visualstudio.com/items?itemName=aaron-bond.better-comments)

## Conventions
1. Use functional components and hooks
2. Minimize the amount of states
3. Use TypeScript
4. Use fragments instead of divs
5. See Comments tab below

## Comments
We'll use the Better Comments extension to write comments. Below is an example.
```
/**
 * updateUserElo
 * * Important information is highlighted.
 * ! Deprecated method, do not use.
 * ? Should this method be exposed in the public API?
 * TODO: refactor this method.
 @prop userID the logged in user's ID
 @prop gameID the ID of the match the user just played
 */
 const updateUserElo = function (userID: number, gameID: number): void {
  // ! Old method is deprecated. In process of changing
  ////let eloChange: number = oldGetEloFromGame(gameID);
  
  // * new method uses Glicko-2 rating system
  // TODO: write tests for elo function
  let eloChange: number = getEloFromGame(gameID);
  
  // * User interface defined in /schemas
  let userObject: User = getUserObject(userID);
  userObject.elo += eloChange;
  
  // ? Should we console.log the new elo afterwards?
 }
```

## Scripts

1. `yarn start`
  - Runs the app in the development mode.
  - Open [http://localhost:3000](http://localhost:3000) to view it in the browser.

2. `yarn test`
  - Launches the test runner in the interactive watch mode.\

3. `yarn build`
  - Builds the app for production to the `build` folder.\
