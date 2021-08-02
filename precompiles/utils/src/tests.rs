// Copyright 2019-2021 PureStake Inc.
// This file is part of Moonbeam.

// Moonbeam is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Moonbeam is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Moonbeam.  If not, see <http://www.gnu.org/licenses/>.

use super::*;
use crate::data::Bytes;
use sp_core::{H256, U256};
fn u256_repeat_byte(byte: u8) -> U256 {
	let value = H256::repeat_byte(byte);

	U256::from_big_endian(value.as_bytes())
}

#[test]
fn write_bool() {
	let value = true;

	let writer_output = EvmDataWriter::new().write(value).build();

	let mut expected_output = [0u8; 32];
	expected_output[31] = 1;

	assert_eq!(writer_output, expected_output);
}

#[test]
fn read_bool() {
	let value = true;

	let writer_output = EvmDataWriter::new().write(value).build();

	let mut reader = EvmDataReader::new(&writer_output);
	let parsed: bool = reader.read().expect("to correctly parse bool");

	assert_eq!(value, parsed);
}

#[test]
fn write_u256() {
	let value = U256::from(42);

	let writer_output = EvmDataWriter::new().write(value).build();

	let mut expected_output = [0u8; 32];
	value.to_big_endian(&mut expected_output);

	assert_eq!(writer_output, expected_output);
}

#[test]
fn read_u256() {
	let value = U256::from(42);
	let writer_output = EvmDataWriter::new().write(value).build();

	let mut reader = EvmDataReader::new(&writer_output);
	let parsed: U256 = reader.read().expect("to correctly parse U256");

	assert_eq!(value, parsed);
}

#[test]
#[should_panic(expected = "to correctly parse U256")]
fn read_u256_too_short() {
	let value = U256::from(42);
	let writer_output = EvmDataWriter::new().write(value).build();

	let mut reader = EvmDataReader::new(&writer_output[0..31]);
	let _: U256 = reader.read().expect("to correctly parse U256");
}

#[test]
fn write_h256() {
	let mut raw = [0u8; 32];
	raw[0] = 42;
	raw[12] = 43;
	raw[31] = 44;

	let value = H256::from(raw);

	let output = EvmDataWriter::new().write(value).build();

	assert_eq!(&output, &raw);
}

#[test]
fn read_h256() {
	let mut raw = [0u8; 32];
	raw[0] = 42;
	raw[12] = 43;
	raw[31] = 44;
	let value = H256::from(raw);
	let writer_output = EvmDataWriter::new().write(value).build();

	let mut reader = EvmDataReader::new(&writer_output);
	let parsed: H256 = reader.read().expect("to correctly parse H256");

	assert_eq!(value, parsed);
}

#[test]
#[should_panic(expected = "to correctly parse H256")]
fn read_h256_too_short() {
	let mut raw = [0u8; 32];
	raw[0] = 42;
	raw[12] = 43;
	raw[31] = 44;
	let value = H256::from(raw);
	let writer_output = EvmDataWriter::new().write(value).build();

	let mut reader = EvmDataReader::new(&writer_output[0..31]);
	let _: H256 = reader.read().expect("to correctly parse H256");
}

#[test]
fn write_address() {
	let value = H160::repeat_byte(0xAA);

	let output = EvmDataWriter::new().write(Address(value)).build();

	assert_eq!(output.len(), 32);
	assert_eq!(&output[12..32], value.as_bytes());
}

#[test]
fn read_u32() {
	let value: u32 = 1000u32;
	let writer_output = EvmDataWriter::new().write(value).build();

	let mut reader = EvmDataReader::new(&writer_output);
	let parsed: u32 = reader.read().expect("to correctly parse u32");

	assert_eq!(value, parsed);
}

#[test]
fn write_u32() {
	let value: u32 = 1000u32;

	let output = EvmDataWriter::new().write(value).build();

	assert_eq!(output.len(), 32);
	assert_eq!(&output[28..32], value.to_be_bytes());
}

#[test]
fn read_u8() {
	let value = 100u8;
	let writer_output = EvmDataWriter::new().write(value).build();

	let mut reader = EvmDataReader::new(&writer_output);
	let parsed: u8 = reader.read().expect("to correctly parse u8");

	assert_eq!(value, parsed);
}

#[test]
fn write_u8() {
	let value = 100u8;

	let output = EvmDataWriter::new().write(value).build();

	assert_eq!(output.len(), 32);
	assert_eq!(output[31], value);
}

#[test]
fn read_address() {
	let value = H160::repeat_byte(0xAA);
	let writer_output = EvmDataWriter::new().write(Address(value)).build();

	let mut reader = EvmDataReader::new(&writer_output);
	let parsed: Address = reader.read().expect("to correctly parse Address");

	assert_eq!(value, parsed.0);
}

#[test]
fn write_h256_array() {
	let array = vec![
		H256::repeat_byte(0x11),
		H256::repeat_byte(0x22),
		H256::repeat_byte(0x33),
		H256::repeat_byte(0x44),
		H256::repeat_byte(0x55),
	];
	let writer_output = EvmDataWriter::new().write(array.clone()).build();
	assert_eq!(writer_output.len(), 0xE0);

	// We can read this "manualy" using simpler functions since arrays are 32-byte aligned.
	let mut reader = EvmDataReader::new(&writer_output);

	assert_eq!(reader.read::<U256>().expect("read offset"), 32.into());
	assert_eq!(reader.read::<U256>().expect("read size"), 5.into());
	assert_eq!(reader.read::<H256>().expect("read 1st"), array[0]);
	assert_eq!(reader.read::<H256>().expect("read 2nd"), array[1]);
	assert_eq!(reader.read::<H256>().expect("read 3rd"), array[2]);
	assert_eq!(reader.read::<H256>().expect("read 4th"), array[3]);
	assert_eq!(reader.read::<H256>().expect("read 5th"), array[4]);
}

#[test]
fn read_h256_array() {
	let array = vec![
		H256::repeat_byte(0x11),
		H256::repeat_byte(0x22),
		H256::repeat_byte(0x33),
		H256::repeat_byte(0x44),
		H256::repeat_byte(0x55),
	];
	let writer_output = EvmDataWriter::new().write(array.clone()).build();

	let mut reader = EvmDataReader::new(&writer_output);
	let parsed: Vec<H256> = reader.read().expect("to correctly parse Vec<H256>");

	assert_eq!(array, parsed);
}

#[test]
fn write_u256_array() {
	let array = vec![
		u256_repeat_byte(0x11),
		u256_repeat_byte(0x22),
		u256_repeat_byte(0x33),
		u256_repeat_byte(0x44),
		u256_repeat_byte(0x55),
	];
	let writer_output = EvmDataWriter::new().write(array.clone()).build();
	assert_eq!(writer_output.len(), 0xE0);

	// We can read this "manualy" using simpler functions since arrays are 32-byte aligned.
	let mut reader = EvmDataReader::new(&writer_output);

	assert_eq!(reader.read::<U256>().expect("read offset"), 32.into());
	assert_eq!(reader.read::<U256>().expect("read size"), 5.into());
	assert_eq!(reader.read::<U256>().expect("read 1st"), array[0]);
	assert_eq!(reader.read::<U256>().expect("read 2nd"), array[1]);
	assert_eq!(reader.read::<U256>().expect("read 3rd"), array[2]);
	assert_eq!(reader.read::<U256>().expect("read 4th"), array[3]);
	assert_eq!(reader.read::<U256>().expect("read 5th"), array[4]);
}

#[test]
fn read_u256_array() {
	let array = vec![
		u256_repeat_byte(0x11),
		u256_repeat_byte(0x22),
		u256_repeat_byte(0x33),
		u256_repeat_byte(0x44),
		u256_repeat_byte(0x55),
	];
	let writer_output = EvmDataWriter::new().write(array.clone()).build();

	let mut reader = EvmDataReader::new(&writer_output);
	let parsed: Vec<U256> = reader.read().expect("to correctly parse Vec<H256>");

	assert_eq!(array, parsed);
}

#[test]
fn write_address_array() {
	let array = vec![
		Address(H160::repeat_byte(0x11)),
		Address(H160::repeat_byte(0x22)),
		Address(H160::repeat_byte(0x33)),
		Address(H160::repeat_byte(0x44)),
		Address(H160::repeat_byte(0x55)),
	];
	let writer_output = EvmDataWriter::new().write(array.clone()).build();

	// We can read this "manualy" using simpler functions since arrays are 32-byte aligned.
	let mut reader = EvmDataReader::new(&writer_output);

	assert_eq!(reader.read::<U256>().expect("read offset"), 32.into());
	assert_eq!(reader.read::<U256>().expect("read size"), 5.into());
	assert_eq!(reader.read::<Address>().expect("read 1st"), array[0]);
	assert_eq!(reader.read::<Address>().expect("read 2nd"), array[1]);
	assert_eq!(reader.read::<Address>().expect("read 3rd"), array[2]);
	assert_eq!(reader.read::<Address>().expect("read 4th"), array[3]);
	assert_eq!(reader.read::<Address>().expect("read 5th"), array[4]);
}

#[test]
fn read_address_array() {
	let array = vec![
		Address(H160::repeat_byte(0x11)),
		Address(H160::repeat_byte(0x22)),
		Address(H160::repeat_byte(0x33)),
		Address(H160::repeat_byte(0x44)),
		Address(H160::repeat_byte(0x55)),
	];
	let writer_output = EvmDataWriter::new().write(array.clone()).build();

	let mut reader = EvmDataReader::new(&writer_output);
	let parsed: Vec<Address> = reader.read().expect("to correctly parse Vec<H256>");

	assert_eq!(array, parsed);
}

#[test]
fn write_bytes() {
	let array: Bytes = [1u8; 50].to_vec().into();
	let writer_output = EvmDataWriter::new().write(array.clone()).build();

	// We can read this "manualy" using simpler functions since arrays are 32-byte aligned.
	let mut reader = EvmDataReader::new(&writer_output);

	assert_eq!(reader.read::<U256>().expect("read offset"), 32.into());
	assert_eq!(reader.read::<U256>().expect("read size"), 50.into());
	assert_eq!(
		[1u8; 50].to_vec(),
		reader.read_raw_bytes(50).expect("read 1st")
	);
}

#[test]
fn read_bytes() {
	let array: Bytes = [1u8; 50].to_vec().into();
	let writer_output = EvmDataWriter::new().write(array.clone()).build();

	let mut reader = EvmDataReader::new(&writer_output);
	let parsed: Vec<u8> = reader
		.read::<Bytes>()
		.expect("to correctly parse Vec<u8>")
		.into();

	assert_eq!([1u8; 50].to_vec(), parsed);
}

#[test]

fn write_multiple_bytes() {
	let array1: Bytes = [1u8; 50].to_vec().into();

	let array2: Bytes = [2u8; 50].to_vec().into();

	let writer_output = EvmDataWriter::new()
		.write(array1.clone())
		.write(array2.clone())
		.build();

	assert_eq!(writer_output.len(), 0x100);
	// We can read this "manualy" using simpler functions since arrays are 32-byte aligned.
	let mut reader = EvmDataReader::new(&writer_output);

	assert_eq!(reader.read::<U256>().expect("read 1st offset"), 0x40.into()); // 0x00
	assert_eq!(reader.read::<U256>().expect("read 2nd offset"), 0xa0.into()); // 0x20
	assert_eq!(reader.read::<U256>().expect("read 1st size"), 50.into()); // 0x40
	assert_eq!(
		[1u8; 50].to_vec(),
		reader.read_raw_bytes(50).expect("read 1st")
	);

	// read remaining zeros
	reader.read_raw_bytes(14).expect("read 1st");

	assert_eq!(reader.read::<U256>().expect("read 2nd size"), 50.into()); // 0xC0
	assert_eq!(
		[2u8; 50].to_vec(),
		reader.read_raw_bytes(50).expect("read 1st")
	);
}

#[test]

fn read_multiple_bytes() {
	let array1: Bytes = [1u8; 50].to_vec().into();

	let array2: Bytes = [2u8; 50].to_vec().into();

	let writer_output = EvmDataWriter::new()
		.write(array1.clone())
		.write(array2.clone())
		.build();

	assert_eq!(writer_output.len(), 0x100);

	let mut reader = EvmDataReader::new(&writer_output);
	let parsed: Vec<u8> = reader
		.read::<Bytes>()
		.expect("to correctly parse Vec<u8>")
		.into();
	assert_eq!(array1, parsed.into());

	let parsed_2: Vec<u8> = reader
		.read::<Bytes>()
		.expect("to correctly parse Vec<u8>")
		.into();
	assert_eq!(array2, parsed_2.into());
}

#[test]
fn read_address_array_size_too_big() {
	let array = vec![
		Address(H160::repeat_byte(0x11)),
		Address(H160::repeat_byte(0x22)),
		Address(H160::repeat_byte(0x33)),
		Address(H160::repeat_byte(0x44)),
		Address(H160::repeat_byte(0x55)),
	];
	let mut writer_output = EvmDataWriter::new().write(array.clone()).build();

	U256::from(6).to_big_endian(&mut writer_output[0x20..0x40]);

	let mut reader = EvmDataReader::new(&writer_output);
	match reader.read::<Vec<Address>>() {
		Ok(_) => panic!("should not parse correctly"),
		Err(ExitError::Other(err)) => assert_eq!(err, "tried to parse H160 out of bounds"),
		Err(_) => panic!("unexpected error"),
	}
}

#[test]
fn write_address_nested_array() {
	let array = vec![
		vec![
			Address(H160::repeat_byte(0x11)),
			Address(H160::repeat_byte(0x22)),
			Address(H160::repeat_byte(0x33)),
		],
		vec![
			Address(H160::repeat_byte(0x44)),
			Address(H160::repeat_byte(0x55)),
		],
	];
	let writer_output = EvmDataWriter::new().write(array.clone()).build();
	assert_eq!(writer_output.len(), 0x160);

	writer_output
		.chunks_exact(32)
		.map(|chunk| H256::from_slice(chunk))
		.for_each(|hash| println!("{:?}", hash));

	// We can read this "manualy" using simpler functions since arrays are 32-byte aligned.
	let mut reader = EvmDataReader::new(&writer_output);

	assert_eq!(reader.read::<U256>().expect("read offset"), 0x20.into()); // 0x00
	assert_eq!(reader.read::<U256>().expect("read size"), 2.into()); // 0x20
	assert_eq!(reader.read::<U256>().expect("read 1st offset"), 0x80.into()); // 0x40
	assert_eq!(
		reader.read::<U256>().expect("read 2st offset"),
		0x100.into()
	); // 0x60
	assert_eq!(reader.read::<U256>().expect("read 1st size"), 3.into()); // 0x80
	assert_eq!(reader.read::<Address>().expect("read 1-1"), array[0][0]); // 0xA0
	assert_eq!(reader.read::<Address>().expect("read 1-2"), array[0][1]); // 0xC0
	assert_eq!(reader.read::<Address>().expect("read 1-3"), array[0][2]); // 0xE0
	assert_eq!(reader.read::<U256>().expect("read 2nd size"), 2.into()); // 0x100
	assert_eq!(reader.read::<Address>().expect("read 2-1"), array[1][0]); // 0x120
	assert_eq!(reader.read::<Address>().expect("read 2-2"), array[1][1]); // 0x140
}

#[test]
fn read_address_nested_array() {
	let array = vec![
		vec![
			Address(H160::repeat_byte(0x11)),
			Address(H160::repeat_byte(0x22)),
			Address(H160::repeat_byte(0x33)),
		],
		vec![
			Address(H160::repeat_byte(0x44)),
			Address(H160::repeat_byte(0x55)),
		],
	];
	let writer_output = EvmDataWriter::new().write(array.clone()).build();

	let mut reader = EvmDataReader::new(&writer_output);
	let parsed: Vec<Vec<Address>> = reader.read().expect("to correctly parse Vec<Vec<Address>>");

	assert_eq!(array, parsed);
}

#[test]

fn write_multiple_arrays() {
	let array1 = vec![
		Address(H160::repeat_byte(0x11)),
		Address(H160::repeat_byte(0x22)),
		Address(H160::repeat_byte(0x33)),
	];

	let array2 = vec![H256::repeat_byte(0x44), H256::repeat_byte(0x55)];

	let writer_output = EvmDataWriter::new()
		.write(array1.clone())
		.write(array2.clone())
		.build();

	assert_eq!(writer_output.len(), 0x120);

	// We can read this "manualy" using simpler functions since arrays are 32-byte aligned.
	let mut reader = EvmDataReader::new(&writer_output);

	assert_eq!(reader.read::<U256>().expect("read 1st offset"), 0x40.into()); // 0x00
	assert_eq!(reader.read::<U256>().expect("read 2nd offset"), 0xc0.into()); // 0x20
	assert_eq!(reader.read::<U256>().expect("read 1st size"), 3.into()); // 0x40
	assert_eq!(reader.read::<Address>().expect("read 1-1"), array1[0]); // 0x60
	assert_eq!(reader.read::<Address>().expect("read 1-2"), array1[1]); // 0x80
	assert_eq!(reader.read::<Address>().expect("read 1-3"), array1[2]); // 0xA0
	assert_eq!(reader.read::<U256>().expect("read 2nd size"), 2.into()); // 0xC0
	assert_eq!(reader.read::<H256>().expect("read 2-1"), array2[0]); // 0xE0
	assert_eq!(reader.read::<H256>().expect("read 2-2"), array2[1]); // 0x100
}

#[test]
fn read_multiple_arrays() {
	let array1 = vec![
		Address(H160::repeat_byte(0x11)),
		Address(H160::repeat_byte(0x22)),
		Address(H160::repeat_byte(0x33)),
	];

	let array2 = vec![H256::repeat_byte(0x44), H256::repeat_byte(0x55)];

	let writer_output = EvmDataWriter::new()
		.write(array1.clone())
		.write(array2.clone())
		.build();

	// offset 0x20
	// offset 0x40
	// size 0x60
	// 3 addresses 0xC0
	// size 0xE0
	// 2 H256 0x120
	assert_eq!(writer_output.len(), 0x120);

	let mut reader = EvmDataReader::new(&writer_output);

	let parsed: Vec<Address> = reader.read().expect("to correctly parse Vec<Address>");
	assert_eq!(array1, parsed);

	let parsed: Vec<H256> = reader.read().expect("to correctly parse Vec<H256>");
	assert_eq!(array2, parsed);
}
