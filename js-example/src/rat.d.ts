/**
 * Type definitions mirroring Analysis.has_redos
 */

/** [has_redos s] returns Safe, Dangerous, or ParseError. */
export type RedosResult = "Safe" | "Dangerous" | "ParseError";

export interface Rat {
  hasRedos(source: string): RedosResult;
}
