using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class BulletSpawner : MonoBehaviour//脚本用于龙王发射水弹
{
    public Transform[] spawnPoints; // 子弹发射点数组
    public GameObject bulletPrefab; // 子弹预制体
    public float spawnInterval = 2f; // 发射间隔

    private void OnEnable()
    {
        if (spawnPoints.Length < 4)
        {
            Debug.LogError("Not enough spawn points provided. Please assign 4 spawn points.");
            return;
        }
        StartCoroutine(SpawnBullets());
    }

    private IEnumerator SpawnBullets()
    {
        while (true)
        {
            yield return new WaitForSeconds(spawnInterval);
            SpawnBullet();
        }
    }

    private void SpawnBullet()
    {
        // 从4个点中随机选择2个不同的点
        int index1 = Random.Range(0, spawnPoints.Length);
        int index2;
        do
        {
            index2 = Random.Range(0, spawnPoints.Length);
        } while (index2 == index1);

        Instantiate(bulletPrefab, spawnPoints[index1].position, Quaternion.identity);
        Instantiate(bulletPrefab, spawnPoints[index2].position, Quaternion.identity);
    }
}
