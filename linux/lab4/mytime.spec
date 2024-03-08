Name:           mytime
Version:        0.0.1
Release:        1%{?dist}
Summary:        A simple time world script
BuildArch:      noarch

License:        GPL
Source0:        %{name}-%{version}.tar.gz

Requires:       bash

%description
Time script

%prep
%setup -q

%install
mkdir -p %{buildroot}/usr/bin/
install -m 755 mytime.sh %{buildroot}/usr/bin/mytime.sh

%clean
rm -rf $RPM_BUILD_ROOT

%files
%{_bindir}/%{name}.sh

%changelog
* Wed Oct 4 2023 Korolev Peter - 0.0.1
- Init

