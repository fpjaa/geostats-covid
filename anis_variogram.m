w1 = readtable('residuals_w1.csv');

x = table2array(w1(:,2:3));
y = table2array(w1(:,4));

cutoff = 10;
figure(1)
v1 = variogram(x, y, 'nrbins', 15, 'maxdist', cutoff, 'plotit', true, 'anisotropy', true);

w2 = readtable('residuals_w2.csv');

x = table2array(w2(:,2:3));
y = table2array(w2(:,4));

figure(2)
v2 = variogram(x, y, 'nrbins', 15, 'maxdist', cutoff, 'plotit', true, 'anisotropy', true);