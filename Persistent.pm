# -*-perl-*-
######################################################################

=head1 NAME

Tie::Persistent  - persistent data structures via tie

=head1 SYNOPSIS

 use Tie::Persistent;

 tie %DB, 'Tie::Persistent', 'file', 'rw'; # read data from 'file'

 # now create/add/modify database
 ...

 untie %DB;			# stores data back into 'file'

 tie %ReadOnly, 'Tie::Persistent', 'file';
 foreach (keys %ReadOnly) {
   print "$_ => $ReadOnly{$_}\n";
 }

=head1 DESCRIPTION

The Persistent package makes working with persistent data real
easy by using the C<tie> interface.

It works by storing data contained in a variable into a file (not
unlike a database). The primary advantage is speed, as the whole
datastructure is kept in memory (which is also a limitation), and, of
course, that you can use arbitrary data structures inside the variable
(unlike DB_File).

If you want to make an arbitrary object persistent, just store its
ref in a scalar tied to 'Tie::Persistent'.

B<Beware>: not every data structure or object can be made persistent.
For example, it may not contain GLOB or CODE refs, as these are not
really dumpable (yet?).

Also, it works only for variables, you cannot use it for file handles.
[A persistent file handle? Hmmm... Hmmm! Let me think about it.
I have a vague idea, I'll have to do some unconscious work on...]

=head1 PARAMETERS

C<tie> %Hash,   'Tie::Persistent', B<file>, B<mode>, I<other...>;

C<tie> @Array,  'Tie::Persistent', B<file>, B<mode>, I<other...>;

C<tie> $Scalar, 'Tie::Persistent', B<file>, B<mode>, I<other...>;

=over 4

=item B<file>

Filename to store the data in. No naming convention is enforced, but I
personally use the suffix 'pdb' for "Perl Data Base" (or "Persistent Data
Base"?). No file locking is done; see the section on locking below.


=item B<mode> (optional)

Same as mode for POSIX fopen() or IO::File::open. Basically a
combination of 'r', 'w', 'a' and '+'. Semantics:

 'r' .... read only. Modifications in the data are not stored back
          into the file. A non-existing file gives an error. This is
          the default if no mode is given.

 'rw' ... read/write. Modifications are stored back, if the file does
          not exist, it is created.

 'w' .... write only. The file is not read, the variable starts out empty.

 'a', '+' ... append. Same as 'w', but creates numbered backup files.

 'ra', 'r+' ... Same as 'rw', but creates numbered backup files.

When some kind of write access is specified, a backup file of the
old dataset is always created. [You'll thank me for that, believe me.]
The reason is simple: when you tie a variable read-write (the contents
get restored from the file), and your program isn't fully debugged
yet, it may die in the middle of some modifications, but the data
will still be written back to the file, possibly leaving them
inconsistent. Then you always have at least the previous version
that you can restore from.

The default backup filenames follow the Emacs notation, i.e. a '~' is
appended; for numbered backup files (specified as 'a' or '+'), an
additional number and a '~' is appended.

For a file 'data.pdb', the normal backup file would be 'data.pdb~' and
the numbered backup files would be 'data.pdb~1~', 'data.pdb~2~' and so
on. The latest backup file is the one with the highest number. The
backup filename format can be overridden, see below.

=item I<other> (optional, experimental)

This can be a reference to another (possibly tied) variable or
a name of another tieable package.

If a ref is given, it is used internally to store the variable data
instead of an anonymous variable ref. This allows to make other tied
datastructures persistent, e.g. you could first tie a hash to
Tie::IxHash to make it order-preserving and then give it to
Tie::Persistent to make it persistent.

A plain name is used to create this tied variable internally. Trailing
arguments are passed to the other tieable package.

Example:

 tie %h, 'Tie::Persistent', 'file', 'rw', 'Tie::IxHash';

or

 tie %ixh, 'Tie::IxHash';
 tie %ph,  'Tie::Persistent', 'file', 'w', \%ixh;
 # you can now use %ixh as an alias for %ph

B<NOTE>: This is an experimental feature. It may or may not work
with other Tie:: packages. I have only tested it with 'Tie::IxHash'.
Please report success or failure.

=back

=head1 LOCKING

The data file is not automatically locked. Locking has to be done
outside of the package. I recommend using a module like
'Lockfile::Simple' for that.


There are typical two scenarios for locking: you either lock just the
'tie' and/or 'untie' calls, but not the data manipulation, or you lock
the whole 'tie' - modify data - 'untie' sequence.


=head1 CONFIGURATION VARIABLES

B<C<$Tie::Persistent::Readable>> controls which format to use to
store the data inside the file. 'false' means to use 'Storable', which
is faster (and the default), 'true' means to use 'Data::Dumper', which
is slower but much more readable and thus meant for debugging.  This
only influences the way the datastructure is I<written>, format detection
on read is automatic.

B<C<$Tie::Persistent::BackupFile>> points to a sub that determines the
backup filename format. It gets the filename as $_[0] and returns the
backup filename. The default is

 sub { "$_[0]~"; }

which is the Emacs backup format. For NT, you might want to change
this to

 sub { "$_[0].bak"; }

or something.

B<C<$Tie::Persistent::NumberedBackupFile>> points to a sub that
determines the numbered backup filename format. It gets the filename
and a number as $_[0] and $_[1] respectively and returns the backup
filename. The default is

 sub { "$_[0]~$_[1]~"; }

which is the extended Emacs backup format.

=head1 NOTES

There is a module called 'Class::Eroot' that intends to do the same
thing but uses a more difficult interface (and also is rather old, not
that this is a bad thing per se), so I think my module could be of
some use.

'Tie::Persistent' uses 'Storable' and 'Data::Dumper' internally, so
these must be installed.

For testing, I use 'Tie::IxHash', but 'make test' still does some
tests if it is not installed.

=head1 BUGS

Numbered backupfile creation might have problems if the filename contains
the first six digits of the speed of light (in m/s).

All other bugs, please tell me!

=head1 AUTHOR

Roland Giersig <Roland.Giersig@bigfoot.com>

=head1 COPYRIGHT

Copyright (c) 1999 Roland Giersig. All rights reserved.  This program
is free software; you can redistribute it and/or modify it under the
same terms as Perl itself.

=head1 SEE ALSO

L<Storable>, L<Data::Dumper>.

=cut

######################################################################

use strict;

package Tie::Persistent;

use vars qw($VERSION);
$VERSION = '0.901';

use Carp;

# we want to be portable
use File::Basename;
use File::Spec;
use Sys::Hostname;

# uses Storable for performance,
# but Data::Dumper is more readable

use Storable;

use Data::Dumper;
$Data::Dumper::Terse  = 0;
$Data::Dumper::Indent = 1;
$Data::Dumper::Purity = 1;

# Configuration vars:

use vars qw($Readable $BackupFile $NumberedBackupFile);
$Readable = 0;			# set to 1 to use Data::Dumper
$BackupFile = sub { "$_[0]~" };	# format of backup file
$NumberedBackupFile = sub { "$_[0]~$_[1]~" }; # format of numbered backup file

#
# all tie constructors delegate the work to the common '_new'
#
sub TIEHASH {
  my $class = shift;
  unshift @_, 'HASH';
  unshift @_, "${class}::Hash";

  goto &_new;
}

sub TIEARRAY {
  my $class = shift;
  unshift @_, 'ARRAY';
  unshift @_, "${class}::Array";

  croak "TIEARRAY not supported prior to perl v5.005"
    if $] < 5.005;

  goto &_new;
}

sub TIESCALAR {
  my $class = shift;
  unshift @_, 'SCALAR';
  unshift @_, "${class}::Scalar";

  goto &_new;
}

#
# import for easier reading
#
*ISA = \&UNIVERSAL::isa;

#
# as suggested by Mark-Jason Dominus
# now we don't have to copy those object data back into the tie...
#
sub Rebind::TIEHASH { $_[1] }

#
# main workhorse
#
sub _new {
  my ($class, $type, $file, $mode, $other) = @_;
  my $self = [];
  bless $self => $class;
  $self->[1]  = $type;		# keep for easier DESTROY
  $self->[2]  = $file;		# must be given
  $self->[3]  = $mode || 'r';	# mode defaults to read-only

  croak "No filename specified"
    if not defined $file;

  use vars qw($PersistentData);
  # used in 'do' to read data stored with Data::Dumper
  local ($PersistentData);

  if ($mode =~ m/[ra+]/) {
    # not write-only, we may have to read data back in...
    if (not -f $file) {
      # cannot read-only (or append) from non-existing file
      croak "Cannot find file $file"
	if (not $mode =~ m/[w+]/);
    } else {
      # file exists; check if we later can write it back
      if ($mode =~ m/[w+a]/) {
	my $fdir = dirname($file);
	croak "Data file dir $fdir is not writeable"
	  if (not -w $fdir);
	croak "Data file $file is not writeable"
	  if (-f $file and not -w $file);
      }

      # now read; first try Storable...
      eval { $PersistentData = retrieve($file) };
      if (not defined $PersistentData) {
	# nope, now try Data::Dumper...
	open FILE, $file
	  or croak "Cannot open file $file: $!";
	my $l = <FILE>;
	close FILE;
	# check filetype
	croak "File $file is not a PersistentData file"
	  if (substr($l, 0, 15) ne '$PersistentData');

	do $file;
      }
      croak "Cannot load file $file: $@"
	if $@;
      confess "?? PersistentData is not a ref "
	if not defined ref($PersistentData);
    }
  }

  # do we have to chain another var in?
  my $objtype;
  my $tied;
  if (defined $other) {
    if (ref $other) {
      croak "Reference is not a $type"
	if not ref($other) eq $type;
      $self->[0] = $other;
    } else {
      $objtype = $other;
    }
  }

  # what type is the read data?
  my $dataref;
  my $datatype;
  if (defined ($PersistentData)) {
    $dataref = ref($PersistentData);
    ($datatype) = grep {ISA($PersistentData, $_)} qw(HASH ARRAY REF SCALAR);
    $objtype ||= $dataref
      if $dataref ne $datatype;
  }

  # now switch depending on type
  if ($type eq 'HASH') {
    # is a var chained in?
    if ($self->[0]) {
      $tied = tied %{$self->[0]};
    } else {
      # no, create one, retieing (sp?) it if necessary...
      my %h;
      $tied = tie %h, $objtype
	if defined $objtype;
      $self->[0] = \%h;
    }
  } elsif ($type eq 'ARRAY') {
    # is a var chained in?
    if ($self->[0]) {
      $tied = tied @{$self->[0]};
    } else {
      # no, create one, retieing (sp?) it if necessary...
      my @a;
      $tied = tie @a, $objtype
	if defined $objtype;
      $self->[0] = \@a;
    }
  } elsif ($type eq 'SCALAR') {
    # is a var chained in?
    if ($self->[0]) {
      $tied = tied ${$self->[0]};
    } else {
      # no, create one, retieing (sp?) it if necessary...
      my $s;
      $tied = tie $s, $objtype
	if defined $objtype;
      $self->[0] = \$s;
    }
  } else {
    confess "Don't know how to handle a $type";
  }

  if (defined ($PersistentData)) {
    # we have to restore data
    my $tiedref = ref($tied);
    my $tiedtype;
    ($tiedtype) = grep {ISA($tied, $_)} qw(HASH ARRAY REF SCALAR)
      if defined $tied;

    croak "Persistent data is not of type $type"
      if ($dataref eq $datatype and $datatype ne $type);
    if ($tied) {
      # the chained var is tied, so we have to cleverly copy
      # the underlying object back in; we don't have to make
      # a real deep copy, the upper layer should be OK, as
      # $PersistentHash was freshly created just for us...

      croak "Tied data type $tiedtype does not match persistent type $datatype"
	if ($tiedtype ne $datatype);
      croak "Cannot copy persistent object $dataref over tied object $tiedref"
	if ($tiedref ne $dataref);

      if ($tiedtype eq 'HASH') {
	%{$tied} = %$PersistentData;
      } elsif ($tiedtype eq 'ARRAY') {
	@{$tied} = @$PersistentData;
      } elsif ($tiedtype eq 'SCALAR' or $tiedtype eq 'REF') {
	${$tied} = $$PersistentData;
      } else {
	confess "Don't know how to copy a $tiedtype object";
      }
    } else {

      croak "Cannot copy persistent data type $dataref into $type variable"
	if ($dataref ne $type);

      # it's a regular var, so we copy the data the normal way...
      if ($type eq 'HASH') {
	%{$self->[0]} = %$PersistentData;
      } elsif ($type eq 'ARRAY') {
	@{$self->[0]} = @$PersistentData;
      } elsif ($type eq 'SCALAR' or $type eq 'REF') {
	${$self->[0]} = $$PersistentData;
      } else {
	confess "Don't know how to copy a $type object";
      }
    }
  }
  return $self;
}

#
# generic destructor; write back data on destroy;
# gets imported to the subpackages.
#
sub DESTROY {
  my $self = shift;
  my $type = $self->[1];
  my $file = $self->[2];
  my $mode = $self->[3];

  # only overwrite if mode says so
  if ($mode =~ m/[aw+]/) {
    # is this portable? couldn't find a suitable File::Tmpfile or something...
    my $tmpfile = "$file." . hostname . ".$$.tmp";

    # switch over variable type
    my $tied;
    if ($type eq 'HASH') {
      $tied = tied %{$self->[0]};
    } elsif ($type eq 'ARRAY') {
      $tied = tied @{$self->[0]};
    } elsif ($type eq 'SCALAR') {
      $tied = tied ${$self->[0]};
    } else {
      confess "Don't know how to handle $type";
    }

    if ($Readable) {
      # Data::Dumper is more readable...
      open DB, ">$tmpfile"
	or warn ("Tie::Persistent::DESTROY: ",
		 "cannot open $tmpfile for writing, DATA NOT STORED: $!\n"),
		   return;
      if ($tied) {
	# for tied vars, we must dump the underlying object...
	print DB Data::Dumper->Dump([$tied], [qw(PersistentData)]);
      } else {
	# regular vars just dump data...
	print DB Data::Dumper->Dump([$self->[0]], [qw(PersistentData)]);
      }
      close DB;
    } else {
      # Storable is faster...
      if ($tied) {
	# for tied vars, we must dump the underlying object...
	Storable::nstore($tied, $tmpfile);
      } else {
	# regular vars just dump data...
	Storable::nstore($self->[0], $tmpfile);
      }
    }

    # create backup files
    if (-f $file) {
      my $backup;
      if ($mode =~ m/[a+]/) {
	# create numbered backup files
	$backup = _find_next_backup_file($file);
      } else {
	# unnumbered backup file
	$backup = &$BackupFile($file);
      }
      if (defined $backup) {
	rename $file, $backup
	  or warn ("Tie::Persistent::DESTROY: ",
		   "cannot backup $file as $backup: $!\n");
      }
    }

    rename $tmpfile, $file
      or warn ("Tie::Persistent::DESTROY: ",
	       "cannot rename $tmpfile to $file: $!\n");
  }
}

#
# find number of next backup file
#
sub _find_next_backup_file($) {
  my $f = shift;
  my $basefile = basename($f);

  my $dir = dirname($f);
  $dir = File::Spec->curdir() if not $dir;

  opendir (DIR, $dir)
    or warn ("Tie::Persistent::_find_next_backup_file: ",
	    "cannot open dir $dir: $!\n"), return undef;

  # now create a RE matching the backupfile format...
  my $nr = -1;
  my $re = quotemeta(&$NumberedBackupFile($basefile, 299792));
  $re =~ s/299792/(\\d)/;

  # find the highest backup number...
  foreach (readdir(DIR)) {
    if (m/\A$re\Z/) {
      $nr = $1 if $nr < $1;
    }
  }
  closedir DIR;
  $nr++;
  return File::Spec->catfile($dir, &$NumberedBackupFile($basefile, $nr));
}

#
# type-specific access functions below
#

package Tie::Persistent::Hash;

sub STORE    { $_[0]->[0]{$_[1]} = $_[2] }
sub FETCH    { $_[0]->[0]{$_[1]} }
sub FIRSTKEY { my $a = scalar keys %{$_[0]->[0]}; each %{$_[0]->[0]} }
sub NEXTKEY  { each %{$_[0]->[0]} }
sub EXISTS   { exists $_[0]->[0]->{$_[1]} }
sub DELETE   { delete $_[0]->[0]->{$_[1]} }
sub CLEAR    { %{$_[0]->[0]} = () }
*DESTROY = \&Tie::Persistent::DESTROY; # import generic DESTROY


package Tie::Persistent::Array;

# I copied these from perl5.00502, but I have not tested them, as I
# have no perl5.005 installed (yet).
# Don't blame me if they don't work (but tell me!).

sub FETCHSIZE { scalar @{$_[0]->[0]} }
sub STORESIZE { $#{$_[0]->[0]} = $_[1]-1 }
sub STORE     { $_[0]->[0][$_[1]] = $_[2] }
sub FETCH     { $_[0]->[0][$_[1]] }
sub CLEAR     { @{$_[0]->[0]} = () }
sub POP       { pop(@{$_[0]->[0]}) }
sub PUSH      { my $o = shift->[0]; push(@$o,@_) }
sub SHIFT     { shift(@{$_[0]->[0]}) }
sub UNSHIFT   { my $o = shift->[0]; unshift(@$o,@_) }
sub EXTEND    { }
*DESTROY = \&Tie::Persistent::DESTROY; # import generic DESTROY

sub SPLICE
{
 my $ob  = shift->[0];
 my $sz  = $ob->FETCHSIZE;
 my $off = @_ ? shift : 0;
 $off   += $sz if $off < 0;
 my $len = @_ ? shift : $sz-$off;
 return splice(@$ob,$off,$len,@_);
}


package Tie::Persistent::Scalar;

sub STORE    { ${$_[0]->[0]} = $_[1]; }
sub FETCH    { ${$_[0]->[0]}; }

*DESTROY = \&Tie::Persistent::DESTROY; # import generic DESTROY

1;

__END__

