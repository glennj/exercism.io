# Translation Service

Welcome to Translation Service on Exercism's JavaScript Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

The [`Promise`][promise-docs] object represents the eventual completion (or failure) of an
asynchronous operation, and its resulting value.

The methods [`promise.then()`][promise-then], [`promise.catch()`][promise-catch], and [`promise.finally()`][promise-finally] are used to associate further action with a promise that becomes settled.

For example:

```javascript
const myPromise = new Promise(function (resolve, reject) {
  let sampleData = [2, 4, 6, 8];
  let randomNumber = Math.ceil(Math.random() * 5);
  if (sampleData[randomNumber]) {
    resolve(sampleData[randomNumber]);
  } else {
    reject('An error occured!');
  }
});

myPromise
  .then(function (e) {
    console.log(e);
  })
  .catch(function (error) {
    throw new Error(error);
  })
  .finally(function () {
    console.log('Promise completed');
  });
```

## Methods

These methods are available on `Promise.prototype`

**then**

> The `.then()` method takes up to two arguments; the first argument is a callback function for the resolved case of the promise, and the second argument is a callback function for the rejected case. Each `.then()` returns a newly generated promise object, which can optionally be used for chaining.[^1]

```javascript
const promise1 = new Promise(function (resolve, reject) {
  resolve('Success!');
});

promise1.then(function (value) {
  console.log(value);
  // expected output: "Success!"
});
```

**catch**

> A `.catch()` is really just a `.then()` without a slot for a callback function for the case when the promise is resolved. It is used to handle rejected promises.[^2]

```javascript
const promise1 = new Promise((resolve, reject) => {
  throw 'An error occured';
});

promise1.catch(function (error) {
  console.error(error);
});
// expected output: An error occured
```

**finally**

> When the promise is settled, i.e either fulfilled or rejected, the specified callback function is executed. This provides a way for code to be run whether the promise was fulfilled successfully or rejected once the Promise has been dealt with.[^3]

```javascript
function findDataById(id) {
  return new Promise(function (resolve, reject) {
    let sampleData = [1, 2, 3, 4, 5];
    if (sampleData[id]) {
      resolve(sampleData[id]);
    } else {
      reject(new Error('Invalid id'));
    }
  });
}

findDataById(4)
  .then(function (response) {
    console.log(response);
  })
  .catch(function (err) {
    console.error(err);
  })
  .finally(function () {
    console.log('Promise completed');
  });
```

---

[^1]: `then`, MDN. https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then
[^2]: `catch`, MDN. https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/catch
[^3]: `finally`, MDN. https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/finally

[promise-docs]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise
[promise-catch]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/catch
[promise-then]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then
[promise-finally]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/finally

## Instructions

In this exercise you'll be providing a `TranslationService` where paid members have some quality assurance.

You have found an out-of-space translation API that is able to fulfill any translation _request_ in a reasonable amount of time, and you want to capitalize on this.

## The API interface

The API has a very minimal interface:

### Fetching a translation

`api.fetch(text)` fetches the translation of `text`, returning two values:

- `translation`: the actual translation
- `quality`: the quality expressed as a number

If there is no translation available (because it has not been requested yet, see below), the API throws a `NotAvailable` error.
An `Untranslatable` error is thrown if a piece of text is untranslatable.

```javascript
api.fetch('jIyaj');
// => Promise({ resolved: 'I understand' })
```

### Requesting a translation

Some translations are known in the future.
The API knows about these.
That's the difference between `NotAvailable` (will be available, but must be requested) and `Untranslatable` (will never be available).

`api.request(text, callback)` requests the translation of `text`, calling the `callback` once it's ready, without a value, only indicating that it is now available.

> This API is _unstable_, which means that sometimes the API will fail and call the `callback` with an error.
> If that happens, it is okay to re-request.

```javascript
api.request('majQa’');
// => Promise({ resolved: undefined })
```

### ⚠ Warning! ⚠

```exercism/caution
The API works its magic by teleporting in the various translators when a `request` comes in.
This is a very costly action, so it shouldn't be called when a translation *is* available.
Unfortunately not everyone reads the manual, so there is a system in place to kick-out bad actors.

If a `api.request` is called for `text` is available, the API throws an `AbusiveClientError` for this call, **and every call after that**.
Ensure that you *never* request a translation if something has already been translated.
```

## 1. Fetch a translation, ignoring the quality

Implement a function `free(text)` to fetch a translation, ignoring the quality, and forwarding any errors thrown by the API:

- Returns the translation if it can be retrieved, regardless its quality
- Forwards any error from the translation API

```javascript
service.free('jIyaj');
// => Promise<...> resolves "I understand."

service.free("jIyajbe'");
// => Promise<...> rejects Error("Not yet translated")
```

## 2. Fetch a batch of translations, all-or-nothing

Implement a function `batch([text, text, ...])` that translates the given texts using the free service, returning all the translations, or a single error.

- Resolves with all the translations (in the same order), if they are all available
- Rejects with the first error that is encountered
- Rejects with a `BatchIsEmpty` error if no texts are given

```javascript
service.batch(['jIyaj', "majQa'"]);
// => Promise<...> resolves ["I understand.", "Well done!"]

service.batch(['jIyaj', "jIyajbe'"]);
// => Promise<...> rejects new Error("Not yet translated")

service.batch([]);
// => Promise<...> rejects BatchIsEmpty()
```

## 3. Request a translation, retrying at most 2 times

Implement a function `request(text)` that _requests_ a translation, with automatic retries, up to a total of **3 calls** for the same request.

- If `api.request` does not return an error, resolve with `undefined`
- If `api.request` returns an error, retry at most two times
- If you're out of retires, reject with the last error received

```javascript
service.request("jIyajbe'");
// => Promise<...> resolves (with nothing), can now be retrieved using the fetch API
```

## 4. Fetch a translation, inspect the quality, or request it

Implement the function `premium(text, quality)` for premium users, which fetches a translation, request it if it's not available, and only returns it if it meets a certain threshold.

- If `api.fetch` resolves, check the quality before resolving
- If `api.fetch` rejects with `NotAvailable`, _request_ the translation instead
- If `api.fetch` rejects with `Untranslatable`, forward the error
- If _requesting_ rejects, forward the error

```javascript
service.premium("jIyajbe'", 100);
// => Promise<...> resolves "I don't understand."

service.premium("'arlogh Qoylu'pu'?", 100);
// => Promise<...> rejects QualityThresholdNotMet()

service.premium("'arlogh Qoylu'pu'?", 40);
// => Promise<...> resolves "What time is it?"
```

## N.B.

```exercism/note
The correct translation of `'arlogh Qoylu'pu'?` is **How many times has it been heard?**.
```

## Source

### Created by

- @SleeplessByte

### Contributed to by

- @AndrewLawendy