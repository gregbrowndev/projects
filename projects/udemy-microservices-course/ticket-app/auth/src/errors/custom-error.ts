export abstract class CustomError extends Error {
  abstract statusCode: number;

  protected constructor(public message: string) {
    super(message); // pass message to ApiError as this is helpful in logging
    Object.setPrototypeOf(this, CustomError.prototype);
  }
}
