/**
 * Type definitions for rat-redos (ReDoS analyzer).
 */

export type RedosResult = "Safe" | "Dangerous" | "ParseError";

export interface Rat {
  hasRedos(source: string): RedosResult;
}

declare const rat: Rat;
export default rat;
