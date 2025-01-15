/*
 * emify() allows for the player's theme color to be shared globally across all components
 @prop sizeValue: number = the desired image scale ("em" implied)
 @prop aspectRatio: number = 
 @returns string = the size value * the aspect ratio (if exists) turned into a string with "em" appended
*/
export function emify(sizeValue: number, aspectRatio?: number): string {
  return (aspectRatio ? sizeValue * aspectRatio : sizeValue) + "em";
}