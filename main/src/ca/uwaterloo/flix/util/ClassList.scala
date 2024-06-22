/*
 * Copyright 2024 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.util

/**
  * A list of classes available on the Java Platform.
  */
object ClassList {

  /**
    * The class list for Java 21.
    *
    * Excludes: (a) all special classes (e.g. lambdas), and (b) all internal classes (e.g. sun classes).
    */
  val TheList: List[String] =
    s"""
       |java/io/BufferedInputStream.class
       |java/io/BufferedOutputStream.class
       |java/io/BufferedWriter.class
       |java/io/ByteArrayInputStream.class
       |java/io/ByteArrayOutputStream.class
       |java/io/Closeable.class
       |java/io/DataInput.class
       |java/io/DataInputStream.class
       |java/io/DataOutput.class
       |java/io/DefaultFileSystem.class
       |java/io/File.class
       |java/io/FileCleanable.class
       |java/io/FileDescriptor.class
       |java/io/FileInputStream.class
       |java/io/FileOutputStream.class
       |java/io/FilePermission.class
       |java/io/FileSystem.class
       |java/io/FilterInputStream.class
       |java/io/FilterOutputStream.class
       |java/io/Flushable.class
       |java/io/InputStream.class
       |java/io/ObjectStreamField.class
       |java/io/OutputStream.class
       |java/io/OutputStreamWriter.class
       |java/io/PrintStream.class
       |java/io/RandomAccessFile.class
       |java/io/Serializable.class
       |java/io/WinNTFileSystem.class
       |java/io/Writer.class
       |java/lang/AbstractStringBuilder.class
       |java/lang/Appendable.class
       |java/lang/ApplicationShutdownHooks.class
       |java/lang/ArithmeticException.class
       |java/lang/ArrayStoreException.class
       |java/lang/AssertionStatusDirectives.class
       |java/lang/AutoCloseable.class
       |java/lang/BaseVirtualThread.class
       |java/lang/Boolean.class
       |java/lang/BootstrapMethodError.class
       |java/lang/Byte.class
       |java/lang/CharSequence.class
       |java/lang/Character.class
       |java/lang/CharacterData.class
       |java/lang/CharacterData00.class
       |java/lang/CharacterDataLatin1.class
       |java/lang/Class.class
       |java/lang/ClassCastException.class
       |java/lang/ClassLoader.class
       |java/lang/ClassNotFoundException.class
       |java/lang/ClassValue.class
       |java/lang/Cloneable.class
       |java/lang/Comparable.class
       |java/lang/CompoundEnumeration.class
       |java/lang/Double.class
       |java/lang/Enum.class
       |java/lang/Error.class
       |java/lang/Exception.class
       |java/lang/Float.class
       |java/lang/IllegalArgumentException.class
       |java/lang/IllegalMonitorStateException.class
       |java/lang/IncompatibleClassChangeError.class
       |java/lang/Integer.class
       |java/lang/InternalError.class
       |java/lang/Iterable.class
       |java/lang/LinkageError.class
       |java/lang/LiveStackFrame.class
       |java/lang/LiveStackFrameInfo.class
       |java/lang/Long.class
       |java/lang/Math.class
       |java/lang/Module.class
       |java/lang/ModuleLayer.class
       |java/lang/NamedPackage.class
       |java/lang/NoClassDefFoundError.class
       |java/lang/NoSuchFieldException.class
       |java/lang/NoSuchMethodError.class
       |java/lang/NoSuchMethodException.class
       |java/lang/NullPointerException.class
       |java/lang/Number.class
       |java/lang/Object.class
       |java/lang/OutOfMemoryError.class
       |java/lang/Package.class
       |java/lang/Readable.class
       |java/lang/Record.class
       |java/lang/ReflectiveOperationException.class
       |java/lang/Runnable.class
       |java/lang/Runtime.class
       |java/lang/RuntimeException.class
       |java/lang/RuntimePermission.class
       |java/lang/SecurityManager.class
       |java/lang/Short.class
       |java/lang/Shutdown.class
       |java/lang/StackFrameInfo.class
       |java/lang/StackOverflowError.class
       |java/lang/StackTraceElement.class
       |java/lang/StackWalker.class
       |java/lang/StrictMath.class
       |java/lang/String.class
       |java/lang/StringBuffer.class
       |java/lang/StringBuilder.class
       |java/lang/StringCoding.class
       |java/lang/StringConcatHelper.class
       |java/lang/StringLatin1.class
       |java/lang/StringUTF16.class
       |java/lang/System.class
       |java/lang/Terminator.class
       |java/lang/Thread.class
       |java/lang/ThreadGroup.class
       |java/lang/ThreadLocal.class
       |java/lang/Throwable.class
       |java/lang/VersionProps.class
       |java/lang/VirtualMachineError.class
       |java/lang/VirtualThread.class
       |java/lang/Void.class
       |java/lang/WeakPairMap.class
       |java/lang/annotation/Annotation.class
       |java/lang/constant/Constable.class
       |java/lang/constant/ConstantDesc.class
       |java/lang/invoke/AbstractValidatingLambdaMetafactory.class
       |java/lang/invoke/BootstrapMethodInvoker.class
       |java/lang/invoke/BoundMethodHandle.class
       |java/lang/invoke/CallSite.class
       |java/lang/invoke/ClassSpecializer.class
       |java/lang/invoke/ConstantCallSite.class
       |java/lang/invoke/DelegatingMethodHandle.class
       |java/lang/invoke/DirectMethodHandle.class
       |java/lang/invoke/InfoFromMemberName.class
       |java/lang/invoke/InnerClassLambdaMetafactory.class
       |java/lang/invoke/InvokerBytecodeGenerator.class
       |java/lang/invoke/Invokers.class
       |java/lang/invoke/LambdaForm.class
       |java/lang/invoke/LambdaFormBuffer.class
       |java/lang/invoke/LambdaFormEditor.class
       |java/lang/invoke/LambdaMetafactory.class
       |java/lang/invoke/LambdaProxyClassArchive.class
       |java/lang/invoke/MemberName.class
       |java/lang/invoke/MethodHandle.class
       |java/lang/invoke/MethodHandleImpl.class
       |java/lang/invoke/MethodHandleInfo.class
       |java/lang/invoke/MethodHandleNatives.class
       |java/lang/invoke/MethodHandleStatics.class
       |java/lang/invoke/MethodHandles.class
       |java/lang/invoke/MethodType.class
       |java/lang/invoke/MethodTypeForm.class
       |java/lang/invoke/MutableCallSite.class
       |java/lang/invoke/ResolvedMethodName.class
       |java/lang/invoke/SimpleMethodHandle.class
       |java/lang/invoke/StringConcatFactory.class
       |java/lang/invoke/TypeConvertingMethodAdapter.class
       |java/lang/invoke/TypeDescriptor.class
       |java/lang/invoke/VarForm.class
       |java/lang/invoke/VarHandle.class
       |java/lang/invoke/VarHandleGuards.class
       |java/lang/invoke/VarHandles.class
       |java/lang/invoke/VolatileCallSite.class
       |java/lang/module/Configuration.class
       |java/lang/module/ModuleDescriptor.class
       |java/lang/module/ModuleFinder.class
       |java/lang/module/ModuleReader.class
       |java/lang/module/ModuleReference.class
       |java/lang/module/ResolvedModule.class
       |java/lang/module/Resolver.class
       |java/lang/ref/Cleaner.class
       |java/lang/ref/FinalReference.class
       |java/lang/ref/Finalizer.class
       |java/lang/ref/NativeReferenceQueue.class
       |java/lang/ref/PhantomReference.class
       |java/lang/ref/Reference.class
       |java/lang/ref/ReferenceQueue.class
       |java/lang/ref/SoftReference.class
       |java/lang/ref/WeakReference.class
       |java/lang/reflect/AccessFlag.class
       |java/lang/reflect/AccessibleObject.class
       |java/lang/reflect/AnnotatedElement.class
       |java/lang/reflect/Array.class
       |java/lang/reflect/ClassFileFormatVersion.class
       |java/lang/reflect/Constructor.class
       |java/lang/reflect/Executable.class
       |java/lang/reflect/Field.class
       |java/lang/reflect/GenericDeclaration.class
       |java/lang/reflect/Member.class
       |java/lang/reflect/Method.class
       |java/lang/reflect/Modifier.class
       |java/lang/reflect/Parameter.class
       |java/lang/reflect/RecordComponent.class
       |java/lang/reflect/ReflectAccess.class
       |java/lang/reflect/Type.class
       |java/math/BigInteger.class
       |java/math/RoundingMode.class
       |java/net/DefaultInterface.class
       |java/net/Inet4Address.class
       |java/net/Inet4AddressImpl.class
       |java/net/Inet6Address.class
       |java/net/Inet6AddressImpl.class
       |java/net/InetAddress.class
       |java/net/InetAddressImpl.class
       |java/net/InterfaceAddress.class
       |java/net/NetworkInterface.class
       |java/net/URI.class
       |java/net/URL.class
       |java/net/URLClassLoader.class
       |java/net/URLStreamHandler.class
       |java/net/URLStreamHandlerFactory.class
       |java/net/spi/InetAddressResolver.class
       |java/nio/Bits.class
       |java/nio/Buffer.class
       |java/nio/ByteBuffer.class
       |java/nio/ByteOrder.class
       |java/nio/CharBuffer.class
       |java/nio/DirectByteBuffer.class
       |java/nio/DirectByteBufferR.class
       |java/nio/DirectIntBufferRU.class
       |java/nio/DirectIntBufferU.class
       |java/nio/DirectLongBufferU.class
       |java/nio/HeapByteBuffer.class
       |java/nio/HeapCharBuffer.class
       |java/nio/IntBuffer.class
       |java/nio/LongBuffer.class
       |java/nio/MappedByteBuffer.class
       |java/nio/charset/Charset.class
       |java/nio/charset/CharsetDecoder.class
       |java/nio/charset/CharsetEncoder.class
       |java/nio/charset/CoderResult.class
       |java/nio/charset/CodingErrorAction.class
       |java/nio/charset/StandardCharsets.class
       |java/nio/charset/spi/CharsetProvider.class
       |java/nio/file/CopyOption.class
       |java/nio/file/FileSystem.class
       |java/nio/file/FileSystems.class
       |java/nio/file/Files.class
       |java/nio/file/LinkOption.class
       |java/nio/file/OpenOption.class
       |java/nio/file/Path.class
       |java/nio/file/Paths.class
       |java/nio/file/StandardOpenOption.class
       |java/nio/file/Watchable.class
       |java/nio/file/attribute/AttributeView.class
       |java/nio/file/attribute/BasicFileAttributeView.class
       |java/nio/file/attribute/BasicFileAttributes.class
       |java/nio/file/attribute/DosFileAttributes.class
       |java/nio/file/attribute/FileAttributeView.class
       |java/nio/file/attribute/FileTime.class
       |java/nio/file/spi/FileSystemProvider.class
       |java/security/AccessControlContext.class
       |java/security/AccessController.class
       |java/security/AllPermission.class
       |java/security/BasicPermission.class
       |java/security/BasicPermissionCollection.class
       |java/security/CodeSource.class
       |java/security/Guard.class
       |java/security/Permission.class
       |java/security/PermissionCollection.class
       |java/security/Permissions.class
       |java/security/Principal.class
       |java/security/PrivilegedAction.class
       |java/security/PrivilegedExceptionAction.class
       |java/security/ProtectionDomain.class
       |java/security/SecureClassLoader.class
       |java/security/Security.class
       |java/security/UnresolvedPermission.class
       |java/security/cert/Certificate.class
       |java/text/DateFormat.class
       |java/text/DateFormatSymbols.class
       |java/text/DecimalFormat.class
       |java/text/DecimalFormatSymbols.class
       |java/text/DigitList.class
       |java/text/DontCareFieldPosition.class
       |java/text/FieldPosition.class
       |java/text/Format.class
       |java/text/NumberFormat.class
       |java/text/SimpleDateFormat.class
       |java/text/spi/BreakIteratorProvider.class
       |java/text/spi/CollatorProvider.class
       |java/text/spi/DateFormatProvider.class
       |java/text/spi/DateFormatSymbolsProvider.class
       |java/text/spi/DecimalFormatSymbolsProvider.class
       |java/text/spi/NumberFormatProvider.class
       |java/time/Clock.class
       |java/time/Duration.class
       |java/time/Instant.class
       |java/time/InstantSource.class
       |java/time/LocalDate.class
       |java/time/LocalDateTime.class
       |java/time/LocalTime.class
       |java/time/Period.class
       |java/time/ZoneId.class
       |java/time/ZoneOffset.class
       |java/time/ZoneRegion.class
       |java/time/chrono/AbstractChronology.class
       |java/time/chrono/ChronoLocalDate.class
       |java/time/chrono/ChronoLocalDateTime.class
       |java/time/chrono/ChronoPeriod.class
       |java/time/chrono/Chronology.class
       |java/time/chrono/IsoChronology.class
       |java/time/format/DateTimeFormatter.class
       |java/time/format/DateTimeFormatterBuilder.class
       |java/time/format/DateTimePrintContext.class
       |java/time/format/DateTimeTextProvider.class
       |java/time/format/DecimalStyle.class
       |java/time/format/ResolverStyle.class
       |java/time/format/SignStyle.class
       |java/time/format/TextStyle.class
       |java/time/temporal/ChronoField.class
       |java/time/temporal/ChronoUnit.class
       |java/time/temporal/IsoFields.class
       |java/time/temporal/JulianFields.class
       |java/time/temporal/Temporal.class
       |java/time/temporal/TemporalAccessor.class
       |java/time/temporal/TemporalAdjuster.class
       |java/time/temporal/TemporalAmount.class
       |java/time/temporal/TemporalField.class
       |java/time/temporal/TemporalQueries.class
       |java/time/temporal/TemporalQuery.class
       |java/time/temporal/TemporalUnit.class
       |java/time/temporal/ValueRange.class
       |java/time/zone/ZoneOffsetTransitionRule.class
       |java/time/zone/ZoneRules.class
       |java/util/AbstractCollection.class
       |java/util/AbstractList.class
       |java/util/AbstractMap.class
       |java/util/AbstractSet.class
       |java/util/ArrayDeque.class
       |java/util/ArrayList.class
       |java/util/Arrays.class
       |java/util/Calendar.class
       |java/util/Collection.class
       |java/util/Collections.class
       |java/util/Comparator.class
       |java/util/Date.class
       |java/util/Deque.class
       |java/util/Dictionary.class
       |java/util/EnumMap.class
       |java/util/EnumSet.class
       |java/util/Enumeration.class
       |java/util/Formattable.class
       |java/util/Formatter.class
       |java/util/GregorianCalendar.class
       |java/util/HashMap.class
       |java/util/HashSet.class
       |java/util/Hashtable.class
       |java/util/HexFormat.class
       |java/util/IdentityHashMap.class
       |java/util/ImmutableCollections.class
       |java/util/Iterator.class
       |java/util/KeyValueHolder.class
       |java/util/LinkedHashMap.class
       |java/util/LinkedHashSet.class
       |java/util/List.class
       |java/util/ListIterator.class
       |java/util/ListResourceBundle.class
       |java/util/Locale.class
       |java/util/Map.class
       |java/util/NavigableMap.class
       |java/util/NavigableSet.class
       |java/util/Objects.class
       |java/util/Optional.class
       |java/util/OptionalInt.class
       |java/util/Properties.class
       |java/util/Queue.class
       |java/util/Random.class
       |java/util/RandomAccess.class
       |java/util/RegularEnumSet.class
       |java/util/ResourceBundle.class
       |java/util/SequencedCollection.class
       |java/util/SequencedMap.class
       |java/util/SequencedSet.class
       |java/util/ServiceLoader.class
       |java/util/Set.class
       |java/util/SortedMap.class
       |java/util/SortedSet.class
       |java/util/Spliterator.class
       |java/util/Spliterators.class
       |java/util/StringJoiner.class
       |java/util/TimSort.class
       |java/util/TimeZone.class
       |java/util/TreeMap.class
       |java/util/WeakHashMap.class
       |java/util/concurrent/AbstractExecutorService.class
       |java/util/concurrent/ConcurrentHashMap.class
       |java/util/concurrent/ConcurrentMap.class
       |java/util/concurrent/ConcurrentNavigableMap.class
       |java/util/concurrent/ConcurrentSkipListMap.class
       |java/util/concurrent/ConcurrentSkipListSet.class
       |java/util/concurrent/CopyOnWriteArrayList.class
       |java/util/concurrent/CountedCompleter.class
       |java/util/concurrent/Executor.class
       |java/util/concurrent/ExecutorService.class
       |java/util/concurrent/ForkJoinPool.class
       |java/util/concurrent/ForkJoinTask.class
       |java/util/concurrent/ForkJoinWorkerThread.class
       |java/util/concurrent/Future.class
       |java/util/concurrent/ThreadFactory.class
       |java/util/concurrent/ThreadLocalRandom.class
       |java/util/concurrent/TimeUnit.class
       |java/util/concurrent/atomic/AtomicInteger.class
       |java/util/concurrent/atomic/AtomicLong.class
       |java/util/concurrent/atomic/LongAdder.class
       |java/util/concurrent/atomic/Striped64.class
       |java/util/concurrent/locks/AbstractOwnableSynchronizer.class
       |java/util/concurrent/locks/AbstractQueuedSynchronizer.class
       |java/util/concurrent/locks/Condition.class
       |java/util/concurrent/locks/Lock.class
       |java/util/concurrent/locks/LockSupport.class
       |java/util/concurrent/locks/ReentrantLock.class
       |java/util/function/BiConsumer.class
       |java/util/function/BiFunction.class
       |java/util/function/BinaryOperator.class
       |java/util/function/Consumer.class
       |java/util/function/Function.class
       |java/util/function/IntConsumer.class
       |java/util/function/IntFunction.class
       |java/util/function/IntPredicate.class
       |java/util/function/Predicate.class
       |java/util/function/Supplier.class
       |java/util/jar/Attributes.class
       |java/util/jar/JarEntry.class
       |java/util/jar/JarFile.class
       |java/util/jar/JarVerifier.class
       |java/util/jar/JavaUtilJarAccessImpl.class
       |java/util/jar/Manifest.class
       |java/util/logging/Handler.class
       |java/util/logging/Level.class
       |java/util/logging/LogManager.class
       |java/util/logging/Logger.class
       |java/util/logging/LoggingPermission.class
       |java/util/random/RandomGenerator.class
       |java/util/regex/ASCII.class
       |java/util/regex/CharPredicates.class
       |java/util/regex/IntHashSet.class
       |java/util/regex/MatchResult.class
       |java/util/regex/Matcher.class
       |java/util/regex/Pattern.class
       |java/util/spi/CalendarDataProvider.class
       |java/util/spi/CurrencyNameProvider.class
       |java/util/spi/LocaleNameProvider.class
       |java/util/spi/LocaleServiceProvider.class
       |java/util/spi/TimeZoneNameProvider.class
       |java/util/stream/AbstractPipeline.class
       |java/util/stream/AbstractTask.class
       |java/util/stream/BaseStream.class
       |java/util/stream/Collector.class
       |java/util/stream/Collectors.class
       |java/util/stream/FindOps.class
       |java/util/stream/ForEachOps.class
       |java/util/stream/IntPipeline.class
       |java/util/stream/IntStream.class
       |java/util/stream/Node.class
       |java/util/stream/Nodes.class
       |java/util/stream/PipelineHelper.class
       |java/util/stream/ReduceOps.class
       |java/util/stream/ReferencePipeline.class
       |java/util/stream/Sink.class
       |java/util/stream/Stream.class
       |java/util/stream/StreamOpFlag.class
       |java/util/stream/StreamShape.class
       |java/util/stream/StreamSupport.class
       |java/util/stream/Streams.class
       |java/util/stream/TerminalOp.class
       |java/util/stream/TerminalSink.class
       |java/util/zip/CRC32.class
       |java/util/zip/Checksum.class
       |java/util/zip/Inflater.class
       |java/util/zip/InflaterInputStream.class
       |java/util/zip/ZipCoder.class
       |java/util/zip/ZipConstants.class
       |java/util/zip/ZipEntry.class
       |java/util/zip/ZipFile.class
       |java/util/zip/ZipUtils.class
       |""".stripMargin.split('\n').toList

}
