export function on_guest_list(guest_list, formal_name) {
  // The honorific is the first word of the formal name. Remove it.
  const last_name = formal_name.replace(/^\S+\s+/, '');
  return guest_list.some((name) => name.endsWith(last_name));
}
