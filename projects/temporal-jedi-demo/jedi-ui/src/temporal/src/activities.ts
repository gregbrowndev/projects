function sleep(ms: number) {
  return new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
}

export function randomInt(min: number, max: number) {
  return Math.floor(Math.random() * (max - min + 1) + min);
}

export async function executeOrder66(id: string): Promise<number> {
  console.log(`Executing order ${id}!`);
  await sleep(5000);
  return randomInt(3, 4);
}

export async function executeOrder67(id: string): Promise<number> {
  console.log(`Executing order ${id}!`);
  await sleep(5000);
  return randomInt(3, 10);
}
