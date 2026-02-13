interface AnalyzeResult {
  vulnerable?: boolean;
  attackLanguage?: string;
  exploit?: string;
  example?: string;
  error?: string;
}

interface Rat {
  analyze(source: string, semantics: string): AnalyzeResult;
}

declare const rat: Rat;
export = rat;
