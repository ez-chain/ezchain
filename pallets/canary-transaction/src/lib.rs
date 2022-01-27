// Copyright 2019-2022 PureStake Inc.
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

//! Canary transaction pallet

#![cfg_attr(not(feature = "std"), no_std)]

use frame_support::pallet;

pub use pallet::*;

#[pallet]
pub mod pallet {
	#[cfg(feature = "std")]
	use frame_support::serde::{Deserialize, Serialize};
	use frame_support::sp_runtime::traits::Hash;
	use frame_support::{dispatch::HasCompact, pallet_prelude::*, transactional};
	use frame_system::pallet_prelude::*;
	use sp_arithmetic::traits::BaseArithmetic;
	use sp_runtime::traits::{CheckedAdd, One};
	use sp_std::vec::Vec;

	#[derive(Clone, Encode, Decode, PartialEq, RuntimeDebug, TypeInfo)]
	#[scale_info(skip_type_params(T))]
	pub struct CanaryTransaction<TransactionId, AccountId> {
		pub nonce: TransactionId,
		pub chain: Chain,
		pub from: AccountId,
		pub to: AccountId,
		pub amount: u64,
	}

	#[derive(Clone, Encode, Decode, PartialEq, RuntimeDebug, TypeInfo)]
	#[scale_info(skip_type_params(T))]
	#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
	pub enum Chain {
		Ethereum,
	}

	/// The Ethereum Chain Id Pallet
	#[pallet::pallet]
	pub struct Pallet<T>(PhantomData<T>);

	/// Configuration trait of this pallet.
	#[pallet::config]
	pub trait Config: frame_system::Config {
		type Event: From<Event<Self>> + IsType<<Self as frame_system::Config>::Event>;

		type TransactionId: Member + Parameter + Default + Copy + HasCompact + BaseArithmetic;

		#[pallet::constant]
		type MaxSenderTransactions: Get<u16>;

		#[pallet::constant]
		type MaxReceiverTransactions: Get<u16>;
	}

	#[pallet::storage]
	#[pallet::getter(fn transaction_counter)]
	pub type TransactionCounter<T: Config> = StorageValue<_, T::TransactionId, ValueQuery>;

	#[pallet::storage]
	#[pallet::getter(fn transactions)]
	pub type Transactions<T: Config> =
		StorageMap<_, Twox64Concat, T::Hash, CanaryTransaction<T::TransactionId, T::AccountId>>;

	#[pallet::storage]
	#[pallet::getter(fn sender_transactions)]
	pub type SenderTransactions<T: Config> =
		StorageMap<_, Twox64Concat, T::AccountId, Vec<T::Hash>, ValueQuery>;

	#[pallet::storage]
	#[pallet::getter(fn receiver_transactions)]
	pub type ReceiverTransactions<T: Config> =
		StorageMap<_, Twox64Concat, T::AccountId, Vec<T::Hash>, ValueQuery>;

	#[pallet::event]
	#[pallet::generate_deposit(pub(super) fn deposit_event)]
	pub enum Event<T: Config> {
		/// A transaction was created. \[transaction_hash, sender, receiver\]
		TransactionCreated(T::Hash, T::AccountId, T::AccountId),
	}

	#[pallet::error]
	pub enum Error<T> {
		/// A math operation lead to an overflow.
		Overflow,
	}

	#[pallet::call]
	impl<T: Config> Pallet<T> {
		#[pallet::weight(100)]
		#[transactional]
		pub fn create(origin: OriginFor<T>, to: T::AccountId, amount: u64) -> DispatchResult {
			let from = ensure_signed(origin)?;
			let id = TransactionCounter::<T>::get();
			let next_id = id.checked_add(&One::one()).ok_or(Error::<T>::Overflow)?;
			TransactionCounter::<T>::put(next_id);
			let transaction = CanaryTransaction {
				nonce: id,
				chain: Chain::Ethereum,
				from: from.clone(),
				to: to.clone(),
				amount,
			};
			let tx_hash = T::Hashing::hash_of(&transaction);
			<SenderTransactions<T>>::mutate(&to, |transactions| {
				if transactions.len() == T::MaxSenderTransactions::get().into() {
					transactions.swap_remove(0);
				}
				transactions.push(tx_hash.clone());
			});
			<ReceiverTransactions<T>>::mutate(&to, |transactions| {
				if transactions.len() == T::MaxReceiverTransactions::get().into() {
					transactions.swap_remove(0);
				}
				transactions.push(tx_hash.clone());
			});
			<Transactions<T>>::insert(tx_hash.clone(), transaction);
			Self::deposit_event(Event::TransactionCreated(tx_hash, from, to));
			Ok(())
		}
	}
}
