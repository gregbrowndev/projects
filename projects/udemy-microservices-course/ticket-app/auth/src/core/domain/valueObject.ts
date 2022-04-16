import { shallowEqual } from 'shallow-equal-object';

/**
 * @desc ValueObjects are objects that we determine their
 * equality through their structural property.
 */
export abstract class ValueObject<T> {
  public readonly props: T;

  protected constructor(props: T) {
    this.props = Object.freeze(props);
  }

  get value(): T {
    return this.props;
  }

  public equals(vo?: ValueObject<T>): boolean {
    if (vo === null || vo === undefined) {
      return false;
    }
    if (vo.props === undefined) {
      return false;
    }
    return shallowEqual(this.props, vo.props);
  }
}
