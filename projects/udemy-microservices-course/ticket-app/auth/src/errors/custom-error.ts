export abstract class CustomError extends Error {
  abstract statusCode: number;

  protected constructor(public message: string) {
    super(message); // pass message to Error as this is helpful in logging
    Object.setPrototypeOf(this, CustomError.prototype);
  }

  abstract serialiseErrors(): { message: string; field?: string }[];
}
