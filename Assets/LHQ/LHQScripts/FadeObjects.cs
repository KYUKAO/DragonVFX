using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Serialization;

namespace LevelArtInitialLHQ
{
    public class FadeObjects : MonoBehaviour //脚本用于哪吒单个姿态透明度过渡
    {
        public float Interval = 1f; // 间隔时间
        public float IntervalOffest = 0.05f;
        private List<GameObject> children = new List<GameObject>();
        private int currentIndex = 0;
        private Vector3 currentPos = new Vector3(0, 0, 0);

        private void Start()
        {
            // 将所有一级子物体放入列表中
            foreach (Transform child in transform)
            {
                children.Add(child.gameObject);
            }

            // 开始按顺序激活子物体
            StartCoroutine(ActivateSequentially());
        }

        IEnumerator ActivateSequentially()
        {
            while (currentIndex < children.Count)
            {
                // 激活当前子物体
                currentPos += new Vector3(0f, 0, -IntervalOffest);
                children[currentIndex].transform.position += currentPos;
                children[currentIndex].SetActive(true);
                // 等待指定的时间间隔
                yield return new WaitForSeconds(Interval);
                currentIndex++;
            }
        }
    }
}