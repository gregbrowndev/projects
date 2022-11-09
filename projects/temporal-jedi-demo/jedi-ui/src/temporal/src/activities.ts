export async function executeOrder(id: string): Promise<string> {
  console.log(`Executing order ${id}!`);
  return id;
}
