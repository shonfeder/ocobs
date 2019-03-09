# OCobs

Consistent Overhead Byte Stuffing for OCaml

> Consistent Overhead Byte Stuffing (COBS) is a framing method for binary
> streams and is useful any time you need to send binary datagrams over a
> stream interface (TCP socket / Serial Port / Etc). In a nutshell, COBS works
> by stripping all `delimiter` bytes (usually `0x00`) out of a binary packet
> and places a single `delimiter` at the end, allowing recipients to simply
> read from the stream until a `delimiter` is encountered (effectively
> allowing a 'readline' like interface for binary data). The encoding/decoding
> are very fast and encoding is guaranteed to only add 1 + max(1, (len/255))
> overhead bytes (making decoding extremely deterministic). For an in-depth
> breakdown of the algorithm, please see
> https://en.wikipedia.org/wiki/Consistent_Overhead_Byte_Stuffing

This project was inspired by [nim_cobs](https://github.com/keyme/nim_cobs),
whence the above description is copied.
